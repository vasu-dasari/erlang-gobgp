/* Copyright (c) 2010-2018, Vasu Dasari <vdasari@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

extern "C" {
  #include "erl_nif.h"
  #include "erl_driver.h"
}

#include <iostream>
#include <string>
#include "gobgp_client.h"

#define Log(...) fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\r\n")

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_undefined;

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, int errnum);

#define REALLOC(bin, nsize) do { \
  size_t osize = bin.size; \
  if (nsize != bin.size) { \
    if (!enif_realloc_binary(&bin, nsize)) \
    return error_tuple(env, ENOMEM); \
    if (nsize > osize) \
    (void)memset(bin.data+osize, 0, bin.size-osize); \
  } \
} while (0);

  static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  Log("Loading Nif");

  Log("gobgp_client intialization %s",
      (GoBgpClient::Self() != NULL) ? "Success" : "Failed");

  atom_ok = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  atom_eagain = enif_make_atom(env, "eagain");
  atom_undefined = enif_make_atom(env, "undefined");

  return (0);
}

/* Stubs for reload and upgrade */
  static int
reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
  Log("Reloading Nif");
  return 0;
}

  static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM info)
{
  Log("Upgrading Nif");
  return 0;
}

  static ERL_NIF_TERM
route_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  char op_str[256] = {0};
  if (!enif_get_atom(env, argv[0], op_str, sizeof(op_str), ERL_NIF_LATIN1))
    return enif_make_badarg(env);

  GoBgpClient::RouteOp OpCode;
  if(strcmp(op_str, "add") == 0) {
    OpCode = GoBgpClient::RouteOp::Add;
  } else if(strcmp(op_str, "delete") == 0) {
    OpCode = GoBgpClient::RouteOp::Add;
  } else {
    return enif_make_badarg(env);
  }

  ErlNifBinary family = {0};
  if (!enif_inspect_iolist_as_binary(env, argv[1], &family))
    return enif_make_badarg(env);

  ErlNifBinary cmd = {0};
  if (!enif_inspect_iolist_as_binary(env, argv[2], &cmd))
    return enif_make_badarg(env);

  REALLOC(family, family.size+1);
  family.data[family.size-1] = '\0';

  REALLOC(cmd, cmd.size+1);
  cmd.data[cmd.size-1] = '\0';

  std::string encoded_bytes;
  std::string ret;
  if ((ret = GoBgpClient::Self()->RouteAnnounce(OpCode,
          (const char*)family.data, (char*)cmd.data, &encoded_bytes)) == "ok") {

    ErlNifBinary buf = {0};

    if (!enif_alloc_binary(encoded_bytes.length(), &buf))
      return error_tuple(env, ENOMEM);
    memcpy(buf.data, encoded_bytes.c_str(), encoded_bytes.length());

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_binary(env, &buf));
  } else {
    return enif_make_tuple2(env,
        atom_error,
        enif_make_string(env, ret.c_str(), ERL_NIF_LATIN1));
  }
}

  static ERL_NIF_TERM
route_family_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary buf = {0};

  if (!enif_inspect_iolist_as_binary(env, argv[0], &buf))
    return enif_make_badarg(env);

  REALLOC(buf, buf.size+1);
  buf.data[buf.size-1] = '\0';

  return enif_make_tuple2(env,
      atom_ok,
      enif_make_int(env, GoBgpClient::Self()->get_route_family((const char*)buf.data)));
}

  static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, int errnum)
{
  return enif_make_tuple2(env, atom_error,
      enif_make_atom(env, erl_errno_id(errnum)));
}

static ErlNifFunc nif_funcs[] = {
  {"route_family", 1, route_family_nif},
  {"route", 3, route_nif}
};

ERL_NIF_INIT(gobgp_nif, nif_funcs, &load, &reload, &upgrade, NULL);
