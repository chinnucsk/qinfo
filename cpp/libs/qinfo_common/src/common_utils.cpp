/// @file   common_utils.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/24/2012 05:01:24 PM

#include "qinfo_common/common_utils.h"
#include "qinfo_common/utf8_encode.h"

#include <boost/scoped_array.hpp>

#include <erl_nif.h>
#include <string.h>
#include <stdio.h>


QINFO_COMMON_UTILS_API ERL_NIF_TERM cp1251_to_unicode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   unsigned int len = 0;
   if (!enif_get_list_length(env, argv[0], &len))
   {
      return enif_make_badarg(env);
   }

   boost::scoped_array<ERL_NIF_TERM> out(new ERL_NIF_TERM[len]);
   ERL_NIF_TERM head;
   ERL_NIF_TERM tail = argv[0];
   int cnt = 0;
   while(enif_get_list_cell(env, tail, &head, &tail))
   {
      int ch;
      enif_get_int(env, head, &ch);
      out[cnt++] = enif_make_int(env, cp1251_to_utf8((unsigned char)ch));
   }
   return enif_make_list_from_array(env, out.get(), len);
}

static ErlNifFunc nif_funcs[] =
{
   {"cp1251_to_unicode", 1, cp1251_to_unicode}
};

ERL_NIF_INIT(qinfo_common_utils, nif_funcs, NULL, NULL, NULL, NULL);
