#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>
#include <sys/param.h>

#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <net/if.h>

#include <errno.h>

#include <sys/ioctl.h>

#include <sys/stat.h>
#include <ctype.h>

#include <sys/uio.h>
#if defined(__sun) && defined(__SVR4)
#include <stropts.h>
#endif

#include "libgobgp.h"
#include <dlfcn.h>

#define Log(...) fprintf(stderr, __VA_ARGS__)

// Create function pointers
typedef path* (*serialize_path_dynamic_t)(int p0, char* p1);
typedef char* (*decode_path_dynamic_t)(path* p0);
typedef int (*get_route_family_fn_t)(const char*);
// typedef int (*get_route_family_t)(const char*);

serialize_path_dynamic_t serialize_path_dynamic = NULL;
decode_path_dynamic_t decode_path_dynamic = NULL;
get_route_family_fn_t get_route_family_fn = NULL;

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

int load_dll(const char* lib_file_name)
{
    void * gobgp_library_handle = dlopen(lib_file_name, RTLD_NOW);
    if (gobgp_library_handle == NULL) {
        Log("Could not load gobgp binary library\n");
        exit(1);
    }

    serialize_path_dynamic = (serialize_path_dynamic_t)dlsym(gobgp_library_handle, "serialize_path");
    if (serialize_path_dynamic == NULL) {
        Log("Could not load function serialize_path from the dynamic library\n");
        exit(1);
    }

    decode_path_dynamic = (decode_path_dynamic_t)dlsym(gobgp_library_handle, "decode_path");
    if (decode_path_dynamic == NULL) {
        Log("Could not load function decode_path from the dynamic library\n");
        exit(1);
    }

    get_route_family_fn = (get_route_family_fn_t)dlsym(gobgp_library_handle, "get_route_family");
    if (get_route_family_fn == NULL) {
        Log("Could not load function decode_path from the dynamic library\n");
        exit(1);
    }

	return 0;
}

int process_command(const char* cmd)
{
	Log("1. Processing command: %s\n", cmd);
    // Log("Route family %d\r]n", dlsym(gobgp_library_handle, "get_route_family")("l2vpn-evpn"));
    Log("Route family %d\r]n", get_route_family_fn("l2vpn-evpdn"));
    // libgobgp.get_route_family("l2vpn-evpn").
	return 0;
}

    static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifBinary buf = {0};

    Log("Loading Nif\n");
    if (!enif_inspect_iolist_as_binary(env, load_info, &buf))
        return enif_make_badarg(env);
    REALLOC(buf, buf.size+1);
    buf.data[buf.size-1] = '\0';

	load_dll((const char*)buf.data);

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
    Log("Reloading Nif\n");
    return 0;
}

    static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM info)
{
    Log("Upgrading Nif\n");
    return 0;
}

    static ERL_NIF_TERM
command_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary buf = {0};

    if (!enif_inspect_iolist_as_binary(env, argv[0], &buf))
        return enif_make_badarg(env);

    REALLOC(buf, buf.size+1);
    buf.data[buf.size-1] = '\0';

	process_command((const char*)buf.data);

    return atom_ok;
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
           enif_make_int(env, get_route_family_fn((const char*)buf.data)));
}

    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, int errnum)
{
    return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errnum)));
}

static ErlNifFunc nif_funcs[] = {
    {"route_family", 1, route_family_nif},
    {"command", 1, command_nif}
};

ERL_NIF_INIT(gobgp_nif, nif_funcs, &load, &reload, &upgrade, NULL);
