/*
 * Copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <erl_nif.h>
#include <string.h>
#include <stdio.h>

#define SSL40

#ifdef SSL40
#define ENIF_ALLOC(SIZE) enif_alloc(SIZE)
#define ENIF_FREE(PTR) enif_free(PTR)
#define ENIF_REALLOC(PTR, SIZE) enif_realloc(PTR, SIZE)
#define ENIF_ALLOC_BINARY(SIZE, BIN) enif_alloc_binary(SIZE, BIN)
#define ENIF_COMPARE(TERM1, TERM2) enif_compare(TERM1, TERM2)
#else
#define ENIF_ALLOC(SIZE) enif_alloc(env, SIZE)
#define ENIF_FREE(PTR) enif_free(env, PTR)
#define ENIF_REALLOC(PTR, SIZE) enif_realloc(env, PTR, SIZE)
#define ENIF_ALLOC_BINARY(SIZE, BIN) enif_alloc_binary(env, SIZE, BIN)
#define ENIF_COMPARE(TERM1, TERM2) enif_compare(env, TERM1, TERM2)
#endif

static ERL_NIF_TERM atom_xmlelement;
static ERL_NIF_TERM atom_xmlcdata;

struct buf {
  int limit;
  int len;
  unsigned char *b;
};

static int make_element(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM el, int is_header);

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  atom_xmlelement = enif_make_atom(env, "xmlel");
  atom_xmlcdata = enif_make_atom(env, "xmlcdata");
  return 0;
}

static struct buf *init_buf(ErlNifEnv* env)
{
  struct buf *rbuf = ENIF_ALLOC(sizeof(struct buf));
  rbuf->limit = 1024;
  rbuf->len = 0;
  rbuf->b = ENIF_ALLOC(rbuf->limit);
  return rbuf;
}

static void destroy_buf(ErlNifEnv* env, struct buf *rbuf)
{
  if (rbuf) {
    if (rbuf->b) {
      ENIF_FREE(rbuf->b);
    };
    ENIF_FREE(rbuf);
  };
}

static void resize_buf(ErlNifEnv* env, struct buf *rbuf, int len_to_add)
{
  int new_len = rbuf->len + len_to_add;
  
  if (new_len > rbuf->limit) {
     while (new_len > rbuf->limit)
	rbuf->limit *= 2;
     rbuf->b = ENIF_REALLOC(rbuf->b, rbuf->limit);
  }
}

static void buf_add_char(ErlNifEnv* env, struct buf *rbuf, unsigned char c)
{
  resize_buf(env, rbuf, 1);
  (rbuf->b)[rbuf->len] = c;
  rbuf->len += 1;
}

static void buf_add_str(ErlNifEnv* env, struct buf *rbuf, char *data, int len)
{
  resize_buf(env, rbuf, len);
  memcpy(rbuf->b + rbuf->len, data, len);
  rbuf->len += len;
}

static void xml_encode(ErlNifEnv* env, struct buf *rbuf, unsigned char *data, int len)
{
  int i;

  for (i = 0; i < len; i++) {
    switch (data[i]) {
    case '&':
      buf_add_str(env, rbuf, "&amp;", 5);
      break;
    case '<':
      buf_add_str(env, rbuf, "&lt;", 4);
      break;
    case '>':
      buf_add_str(env, rbuf, "&gt;", 4);
      break;
    case '"':
      buf_add_str(env, rbuf, "&quot;", 6);
      break;
    case '\'':
      buf_add_str(env, rbuf, "&apos;", 6);
      break;
    default:
      buf_add_char(env, rbuf, data[i]);
      break;
    };
  };
}

static void attr_encode(ErlNifEnv* env, struct buf *rbuf, unsigned char *data, int len)
{
  int i;

  for (i = 0; i < len; i++) {
    switch (data[i]) {
    case '&':
      buf_add_str(env, rbuf, "&amp;", 5);
      break;
    case '<':
      buf_add_str(env, rbuf, "&lt;", 4);
      break;
    case '"':
      buf_add_str(env, rbuf, "&quot;", 6);
      break;
    case '\'':
      buf_add_str(env, rbuf, "&apos;", 6);
      break;
    case '\t':
      buf_add_str(env, rbuf, "&#x9;", 5);
      break;
    case '\n':
      buf_add_str(env, rbuf, "&#xA;", 5);
      break;
    case '\r':
      buf_add_str(env, rbuf, "&#xD;", 5);
      break;
    default:
      buf_add_char(env, rbuf, data[i]);
      break;
    };
  };
}

static int make_elements(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM els)
{
  ERL_NIF_TERM head, tail;
  int ret = 0;

  while (enif_get_list_cell(env, els, &head, &tail)) {
    ret = make_element(env, rbuf, head, 0);
    if (ret) {
      els = tail;
    } else {
      break;
    };
  };

  return ret;
}

static int make_attrs(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM attrs)
{
  ErlNifBinary name, data;
  ERL_NIF_TERM head, tail;
  const ERL_NIF_TERM *tuple;
  int arity, ret = 1;
  
  while (enif_get_list_cell(env, attrs, &head, &tail)) {
    if (enif_get_tuple(env, head, &arity, &tuple)) {
      if (arity == 2) {
	if (enif_inspect_iolist_as_binary(env, tuple[0], &name) &&
	    enif_inspect_iolist_as_binary(env, tuple[1], &data)) {
	  buf_add_char(env, rbuf, ' ');
	  buf_add_str(env, rbuf, (char *)name.data, name.size);
	  buf_add_str(env, rbuf, "='", 2);
	  attr_encode(env, rbuf, data.data, data.size);
	  buf_add_char(env, rbuf, '\'');
	  attrs = tail;
	} else {
	  ret = 0;
	  break;
	};
      } else {
	ret = 0;
	break;
      };
    } else {
      ret = 0;
      break;
    };
  };
  
  return ret;
}

static int make_element(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM el, int is_header)
{
  ErlNifBinary cdata, name;
  const ERL_NIF_TERM *tuple;
  int arity, ret = 0;

  if (enif_get_tuple(env, el, &arity, &tuple)) {
    if (arity == 2 && !is_header) {
      if (!ENIF_COMPARE(tuple[0], atom_xmlcdata)) {
	if (enif_inspect_iolist_as_binary(env, tuple[1], &cdata)) {
	  xml_encode(env, rbuf, cdata.data, cdata.size);
	  ret = 1;
	};
      };
    };
    if (arity == 4) {
      if (!ENIF_COMPARE(tuple[0], atom_xmlelement)) {
	if (enif_inspect_iolist_as_binary(env, tuple[1], &name)) {
	  if (is_header)
	    buf_add_str(env, rbuf, "<?xml version='1.0'?>", 21);
	  buf_add_char(env, rbuf, '<');
	  buf_add_str(env, rbuf, (char *)name.data, name.size);
	  ret = make_attrs(env, rbuf, tuple[2]);
	  if (ret) {
	    if (is_header) {
	      buf_add_char(env, rbuf, '>');
	    } else if (enif_is_empty_list(env, tuple[3])) {
	      buf_add_str(env, rbuf, "/>", 2);
	    } else {
	      buf_add_char(env, rbuf, '>');
	      ret = make_elements(env, rbuf, tuple[3]);
	      if (ret) {
		buf_add_str(env, rbuf, "</", 2);
		buf_add_str(env, rbuf, (char*)name.data, name.size);
		buf_add_char(env, rbuf, '>');
	      };
	    };
	  };
	};
      };
    };
  };
  
  return ret;
}

static ERL_NIF_TERM element_to(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[],
			       int is_header)
{
  ErlNifBinary output;
  ERL_NIF_TERM result;
  struct buf *rbuf;

  if (argc == 1) {
    rbuf = init_buf(env);
    if (make_element(env, rbuf, argv[0], is_header)) {
      if (ENIF_ALLOC_BINARY(rbuf->len, &output)) {
	memcpy(output.data, rbuf->b, rbuf->len);
	result = enif_make_binary(env, &output);
	destroy_buf(env, rbuf);
	return result;
      };
    };
    destroy_buf(env, rbuf);
  };
  
  return enif_make_badarg(env);
}

static ERL_NIF_TERM element_to_binary(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
  return element_to(env, argc, argv, 0);
}

static ERL_NIF_TERM element_to_header(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
  return element_to(env, argc, argv, 1);
}

static ErlNifFunc nif_funcs[] =
  {
    {"element_to_binary", 1, element_to_binary},
    {"element_to_header", 1, element_to_header}
  };

ERL_NIF_INIT(fxml, nif_funcs, load, NULL, NULL, NULL)
