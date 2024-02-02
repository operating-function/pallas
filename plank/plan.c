#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////////////
//  Typedefs

typedef uint64_t u64;

typedef enum Type {
  PIN,
  LAW,
  APP,
  NAT,
  HOL
} Type;

typedef enum NatType {
  SMALL,
  BIG
} NatType;

typedef struct Nat {
  NatType type;
  union {
    u64 direct;
    struct {
      u64 size;
      u64 *buf;
    };
  };
} Nat;

struct Value;

typedef struct Law {
  Nat n;
  Nat a;
  struct Value * b;
} Law;

typedef struct App {
  struct Value * f;
  struct Value * g;
} App;

typedef struct Value {
  Type type;
  union {
    struct Value * p;
    Law l;
    App a;
    Nat n;
  };
} Value;

////////////////////////////////////////////////////////////////////////////////
//  Printing

static const bool tracing = false;

int print_depth = 0;

void crash(char * s) {
  printf("Error: %s\n", s);
  exit(1);
}

void trace_print(const char * format, ...) {
  int d = print_depth*2;
  if (tracing) {
    for (int i=0; i<d; i++) putchar(' ');
    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
  }
}

#define VERBOSE_TRACING 0

#if VERBOSE_TRACING
#define trace_verbose(...) trace_print(__VA_ARGS__)
#else
#define trace_verbose(...) ;
#endif

void check_nat(Nat n) {
    return;
}

void check_value(Value *v) {
  switch (v->type) {
    case PIN:
      check_value(v->p);
      break;
    case LAW:
      check_nat(v->l.n);
      check_nat(v->l.a);
      check_value(v->l.b);
      break;
    case APP:
      check_value(v->a.f);
      check_value(v->a.g);
      break;
    case NAT:
      check_nat(v->n);
      break;
    case HOL:
      break;
    default:
      crash("BAD VALUE TAG");
  }
}

void print_value_internal(Value*, char*, int);

char * print_value_t(Value * v) {
  if (tracing) {
    char * buf = malloc(4096*sizeof(char));
    print_value_internal(v, buf, 0);
    return buf;
  }
}

char * print_value(Value * v) {
  char * buf = malloc(4096*sizeof(char));
  print_value_internal(v, buf, 0);
  return buf;
}

void print_value_app(Value * v, char * buf, int recur) {
  if (v->type != APP) {
    return  print_value_internal(v, buf, recur);
  }
  print_value_app(v->a.f, buf, recur);
  sprintf(buf + strlen(buf), " ");
  print_value_internal(v->a.g, buf, recur+1);
}

void print_nat_internal(Nat, char *);

void print_value_internal(Value * v, char * buf, int recur) {
  if (recur > 10) {
    sprintf(buf + strlen(buf), "‥");
    return;
  }
  switch (v->type) {
    case PIN:
      sprintf(buf + strlen(buf), "<");
      print_value_internal(v->p, buf, recur+1);
      sprintf(buf + strlen(buf), ">");
      break;
    case LAW:
      sprintf(buf + strlen(buf), "{");
      print_nat_internal(v->l.n, buf);
      sprintf(buf + strlen(buf), " ");
      print_nat_internal(v->l.a, buf);
      sprintf(buf + strlen(buf), " ");
      print_value_internal(v->l.b, buf, recur+1);
      sprintf(buf + strlen(buf), "}");
      break;
    case APP:
      sprintf(buf + strlen(buf), "(");
      print_value_app(v, buf, recur+1);
      sprintf(buf + strlen(buf), ")");
      break;
    case NAT:
      print_nat_internal(v->n, buf);
      break;
    case HOL:
      sprintf(buf + strlen(buf), "<>");
      break;
  }
}

char * print_nat(Nat n) {
  char * buf = malloc(512*sizeof(char));
  print_nat_internal(n, buf);
  return buf;
}

static inline bool issym (char c) {
  return (c == '_' || isalnum(c));
}

bool is_symbol (const char *str) {
  if (str[0] == 0) return false;
  if (str[1] == 0) return isalpha(str[0]);
 again:
  char c = *str;
  if (!c) return true;
  if (!issym(c)) return false;
  str++;
  goto again;
}

void print_nat_internal(Nat n, char * buf) {
  switch (n.type) {
    case SMALL:
      char tmp[9] = {0};
      ((u64*)tmp)[0] = n.direct;
      if (is_symbol(tmp)) {
        buf[strlen(buf)] = '%';
        strcpy(buf + strlen(buf), tmp);
      } else {
        sprintf(buf + strlen(buf), "%llu", n.direct);
      }
      break;
    case BIG:
      sprintf(buf + strlen(buf), "[");
      sprintf(buf + strlen(buf), "%llu", n.buf[0]);
      for (int i=1; i<n.size; i++) {
        sprintf(buf + strlen(buf), " %llu", n.buf[i]);
      }
      sprintf(buf + strlen(buf), "]");
      break;
  }
}

////////////////////////////////////////////////////////////////////////////////
//  Construction

Nat d_Nat(u64 n) {
  return (Nat){.type = SMALL, .direct = n};
}

Value * a_Nat(u64 n) {
  Value * res = (Value *)malloc(sizeof(Value));
  res->type = NAT;
  res->n = d_Nat(n);
  return res;
}

Value * a_Big(Nat n) {
  Value * res = (Value *)malloc(sizeof(Value));
  res->type = NAT;
  res->n = n;
  return res;
}

Value * a_Pin(Value * v) {
  Value * res = (Value *)malloc(sizeof(Value));
  res->type = PIN;
  res->p = v;
  return res;
}

Value * a_Law(Nat n, Nat a, Value * b) {
  Value * res = (Value *)malloc(sizeof(Value));
  res->type = LAW;
  res->l.n = n;
  res->l.a = a;
  res->l.b = b;
  return res;
}

Value * a_App(Value * f, Value * g) {
  Value * res = (Value *)malloc(sizeof(Value));
  res->type = APP;
  res->a.f = f;
  res->a.g = g;
  return res;
}

Value * a_Hol() {
  Value * res = (Value *)malloc(sizeof(Value));
  res->type = HOL;
  return res;
}

////////////////////////////////////////////////////////////////////////////////
//  Nat Operators

bool EQ(Nat a, Nat b) {
  if ((a.type == SMALL) && b.type == SMALL)
    return (a.direct == b.direct);
  if ((a.type == BIG) && b.type == BIG) {
    if (a.size != b.size) return false;
    for (int i=0; i<a.size; i++) {
      if (a.buf[i] != b.buf[i]) return false;
    }
    return true;
  }
  return false;
}

bool NEQ(Nat a, Nat b) {
  return !(EQ(a, b));
}

bool LT(Nat a, Nat b) {
  if ((a.type == SMALL) && b.type == SMALL)
    return (a.direct < b.direct);
  if ((a.type == BIG) && b.type == SMALL) return false;
  if ((a.type == SMALL) && b.type == BIG) return true;
  if ((a.type == BIG) && b.type == BIG) {
    if (a.size != b.size) return (a.size < b.size);
    for (int i=(a.size-1); i>=0; i--) {
      if (a.buf[i] == b.buf[i]) continue;
      return (a.buf[i] < b.buf[i]);
    }
    return false;
  }
}

bool GT(Nat a, Nat b) {
  if ((a.type == SMALL) && b.type == SMALL)
    return (a.direct > b.direct);
  if ((a.type == BIG) && b.type == SMALL) return true;
  if ((a.type == SMALL) && b.type == BIG) return false;
  if ((a.type == BIG) && b.type == BIG) {
    if (a.size != b.size) return (a.size > b.size);
    for (int i=(a.size-1); i>=0; i--) {
      if (a.buf[i] == b.buf[i]) continue;
      return (a.buf[i] > b.buf[i]);
    }
    return false;
  }
}

bool LTE(Nat a, Nat b) {
  return !(GT(a, b));
}

bool GTE(Nat a, Nat b) {
  return !(LT(a, b));
}

// just to silence warnings
static inline void *realloc_(void *ptr, size_t sz) {
  void *res = realloc(ptr, sz);
  if (!res) {
    perror("realloc");
    exit(1);
  }
  return res;
}

Nat Inc(Nat n) {
  switch(n.type) {
    case SMALL:
      if (n.direct == UINT64_MAX) {
        u64 * buf = malloc(2*sizeof(u64));
        buf[0] = 0;
        buf[1] = 1;
        return (Nat){ .type = BIG, .size = 2, .buf = buf };
      }
      return (Nat){ .type = SMALL, .direct = (n.direct+1) };
    case BIG:
      int i = 0;
      u64  new_size = n.size;
      u64  *new_buf = malloc(n.size * sizeof(u64));
      new_buf = memcpy(new_buf, n.buf, n.size*sizeof(u64));
      while (i < n.size) {
        if (n.buf[i] == UINT64_MAX) {
          new_buf[i] = 0;
          i++;
          continue;
        } else {
          new_buf[i] = n.buf[i] + 1;
          i++;
          break;
        }
      }
      if (i == n.size) {
        new_size++;
        realloc_(new_buf, new_size * sizeof(u64));
        new_buf[i] = 1;
      }
      return (Nat){ .type = BIG, .size = new_size, .buf = new_buf };
  }
}

Nat Dec(Nat n) {
  switch(n.type) {
    case SMALL:
      if (n.direct == 0) {
        crash("decrement underflow");
      }
      return (Nat){ .type = SMALL, .direct = (n.direct-1) };
    case BIG:
      int i = 0;
      u64  new_size = n.size;
      u64  *new_buf = malloc(n.size * sizeof(u64));
      new_buf = memcpy(new_buf, n.buf, n.size*sizeof(u64));
      while (i < n.size) {
        if (n.buf[i] == 0) {
          new_buf[i] = UINT64_MAX;
          i++;
          continue;
        } else {
          new_buf[i] = n.buf[i] - 1;
          i++;
          break;
        }
      }
      if ((i == n.size) && (new_buf[i-1] == 0)){
        new_size--;
        realloc_(new_buf, new_size * sizeof(u64));
        new_buf[i] = UINT64_MAX;
      }
      return (Nat){ .type = BIG, .size = new_size, .buf = new_buf };

  }
}

Nat Sub(Nat a, Nat b) {
  if ((a.type == SMALL) && (b.type == SMALL)) {
    if (a.direct < b.direct) crash("subtract underflow");
    return (Nat){ .type = SMALL, .direct = (a.direct - b.direct) };
  }
  if ((a.type == SMALL) && (b.type == BIG)) crash("subtract underflow");

  u64 new_size = a.size;
  u64 * new_buf = malloc(new_size * sizeof(u64));
  new_buf = memcpy(new_buf, a.buf, new_size*sizeof(u64));

  if ((a.type == BIG) && (b.type == SMALL)) {
    u64 * b_buf = malloc(sizeof(u64));
    *b_buf = b.direct;
    b = (Nat){ .type = BIG, .size = 1, .buf = b_buf };
  }
  if (a.size < b.size) crash("subtract underflow");

  for (int i=0; i<b.size; i++) {
    if (new_buf[i] < b.buf[i]) {
      new_buf[i] = UINT64_MAX - ((b.buf[i] - (new_buf[i] + 1)));
      int c = i + 1;
      while (true) {
        if (c >= new_size) {
          crash("subtract underflow");
        }
        if (new_buf[c] == 0) {
          new_buf[c] = UINT64_MAX;
          c++;
        } else {
          new_buf[c] = new_buf[c] - 1;
          c++;
          break;
        }
      }
      if ((c == new_size) && (new_buf[c-1] == 0)){
        new_size--;
        realloc_(new_buf, new_size * sizeof(u64));
      }

    } else {
      new_buf[i] = new_buf[i] - b.buf[i];
    }
  }
  return (Nat){ .type = BIG, .size = new_size, .buf = new_buf };
}

////////////////////////////////////////////////////////////////////////////////
//  Combinator

Value * F(Value * o);
Value * E(Value * o);
Value * L(Value * n, Value * e, Value * v, Value * b);
Value * R(Value * n, Value * e, Value * b);

Value * I(Value * f, Value * e, Value * n) {
  trace_verbose("I[%s, %s, %s]\n", print_value_t(f), print_value_t(e), print_value_t(n));

  if (EQ(n->n, d_Nat(0)) && e->type == APP) {
    return e->a.g;
  } else if (EQ(n->n, d_Nat(0)) && e->type != APP) {
    return e;
  } else if (NEQ(n->n, d_Nat(0)) && e->type == APP) {
    return I(f, e->a.f, a_Big(Dec(n->n)));
  } else if (NEQ(n->n, d_Nat(0)) && e->type != APP) {
    return f;
  }
}

Value * A_(Value * o) {
  switch (o->type) {
    case APP:
      Value * head = A_(o->a.f);
      if (EQ(head->n, d_Nat(0))) return a_Nat(0);
      return a_Big(Dec(head->n));
    case PIN:
      return A_(o->p);
    case LAW:
      return a_Big(o->l.a);
    case NAT:
      Value * res = a_Nat(1);
      if (o->n.type == BIG) return res;
      switch (o->n.direct) {
        case 0:   res = a_Nat(3); break;
        case 1:   res = a_Nat(5); break;
        case 2:   res = a_Nat(3); break;
        default:  break;
      }
      return res;
    case HOL:
      crash("<<loop>>");
  }
}

Value * A(Value * o) {
  trace_verbose("A[%s]", print_value_t(o));
  Value * res = A_(o);
  // if (tracing) printf(" ==> %s\n", print_value_t(res));
  return res;
}

Value * N(Value * o) {
  trace_verbose("N[%s]\n", print_value_t(o));
  Value * norm = E(o);
  if (norm->type == NAT) return norm;
  return a_Nat(0);
}

Value * L(Value * n, Value * e, Value * v, Value * b) {
  trace_verbose("L[%s, %s, %s, %s]\n", print_value_t(n), print_value_t(e), print_value_t(v), print_value_t(b));
  Value * x = a_Hol();
  Value * f = a_App(e, x);
  *x = *R(a_Big(Inc(n->n)), f, v);
  return R(a_Big(Inc(n->n)), f, b);
}

Value * R(Value * n, Value * e, Value * b){
  trace_verbose("R[%s, %s, %s]\n", print_value_t(n), print_value_t(e), print_value_t(b));
  if (b->type == NAT && LTE(b->n, n->n)) {
    return I(b, e, a_Big(Sub(n->n, b->n)));
  }
  if (b->type == APP) {
    if (b->a.f->type == APP) {
      if ((b->a.f->a.f->type == NAT) && EQ(b->a.f->a.f->n, d_Nat(0))) {
        Value * f = b->a.f->a.g;
        Value * x = b->a.g;
        return a_App(R(n, e, f), R(n, e, x));
      } else if ((b->a.f->a.f->type == NAT) && EQ(b->a.f->a.f->n, d_Nat(1))) {
        Value * f = b->a.f->a.g;
        Value * x = b->a.g;
        return L(n, e, f, x);
      }
    } else if ((b->a.f->type == NAT) && EQ(b->a.f->n, d_Nat(2))) {
        Value * x = b->a.g;
        return x;
    }
  }
  return b;
}

Value * C(Value * z, Value * p, Value * n) {
  trace_verbose("C[%s, %s, %s]\n", print_value_t(z), print_value_t(p), print_value_t(n));
  if (EQ(n->n, d_Nat(0))) {
    return z;
  } else {
    return a_App(p, a_Big(Dec(n->n)));
  }
}

Value * P(Value * p, Value * l, Value * a, Value * n, Value * o) {
  trace_verbose("P[%s, %s, %s, %s, %s]\n", print_value_t(p), print_value_t(l), print_value_t(a), print_value_t(n), print_value_t(o));
  switch (o->type) {
    case APP:
      return a_App(a_App(a, o->a.f), o->a.g);
    case PIN:
      return a_App(p, o->p);
    case LAW:
      return a_App(a_App(a_App(l, a_Big(o->l.n)), a_Big(o->l.a)), o->l.b);
    case NAT:
      return a_App(n, o);
    case HOL:
      crash("<<loop>>");
  }
}

Value * S(Value * o) {
  trace_verbose("S[%s]\n", print_value_t(o));
  if (o->type == APP) {
    switch (o->a.f->type) {
      case APP:
        return a_App(S(o->a.f), o->a.g);
      case PIN:
        if (o->a.f->p->type != LAW) {
          return S(a_App(o->a.f->p, o->a.g));
        }
    }
  }
  return o;
}

Value * X(Value * k, Value * e) {
  trace_verbose("X[%s]\n", print_value_t(k));
  switch (k->type) {
    case APP:
      return X(k->a.f, e);
    case PIN:
      return X(k->p, e);
  }

  trace_print("<X %s>\n", print_value_t(e));

  switch (k->type) {
    case LAW:
      return R(a_Big(k->l.a), e, k->l.b);
    case HOL:
      crash("<<loop>>");
  }

  Value *b, *a, *n, *p, *l, *x, *z;
  if (k->n.type == BIG) crash(print_value(e));
  switch (k->n.direct) {
    case 0:
      b = e->a.g;
      a = e->a.f->a.g;
      n = e->a.f->a.f->a.g;
      Value * nn = N(n);
      trace_print("<makeLaw %s>\n", print_value(nn));
      print_depth++;
      Value *r = a_Law(nn->n, N(a)->n, F(b));
      print_depth--;
      trace_print("</makeLaw %s>\n", print_value(nn));
      return r;
    case 1:
      x = e->a.g;
      n = e->a.f->a.g;
      a = e->a.f->a.f->a.g;
      l = e->a.f->a.f->a.f->a.g;
      p = e->a.f->a.f->a.f->a.f->a.g;
      return P(p, l, a, n, E(x));
    case 2:
      x = e->a.g;
      p = e->a.f->a.g;
      z = e->a.f->a.f->a.g;
      return C(z, p, N(x));
    case 3:
      x = e->a.g;
      return a_Big(Inc(N(x)->n));
    case 4:
      x = e->a.g;
      return a_Pin(F(x));
  }
  crash(print_value(e));
}

Value * E(Value * o) {
  trace_verbose("E[%s]\n", print_value_t(o));
  switch (o->type) {
    case NAT:
      return o;
    case PIN:
      return o;
    case HOL:
      crash("<<loop>>");
    case APP:
      print_depth++;
      E(o->a.f);
      Value * arity = A(o->a.f);
      if (EQ(arity->n, d_Nat(1))) {
        *o = *S(o);
        trace_verbose("S[] ==>  %s\n", print_value_t(o));
        *o = *X(o,o);
        trace_print("</X %s>\n", print_value(o));
        E(o);
      }
      print_depth--;
      trace_verbose("E[] ==>  %s\n", print_value_t(o));
      return o;
    case LAW:
      if (GT(o->l.a, d_Nat(0))) { return o; }
      print_depth++;
      Value *b = o->l.b;
      o->type = HOL;
      Value o1 = *R(a_Nat(0), o, b);
      *o = o1;
      print_depth--;
      return E(o);
  }
}

Value * F(Value * o) {
  trace_verbose("F[%s]\n", print_value_t(o));
  o = E(o);
  if (o->type == APP) {
    F(o->a.f);
    F(o->a.g);
  }
  return o;
}


////////////////////////////////////////////////////////////////////////////////
//  Seeds

Value * frag_load(Value **tab, u64 tabSz, int *, u64 *, u64 **);

Value * frag_load_cell(Value **tab, u64 tabSz, int *use, u64 *acc, u64 **mor) {
  Value *f = frag_load(tab, tabSz, use, acc, mor);
  Value *x = frag_load(tab, tabSz, use, acc, mor);
  return a_App(f,x);
}

u64 u64_bits (u64 w) {
  if (!w) { return 0; }
  return 64 - __builtin_clzll(w);
}

Value * frag_load(Value **tab, u64 tabSz, int *use, u64 *acc, u64 **mor) {
  u64 isCell = ((*acc >> *use) & 1ULL);

  // move forward by one bit.
  (*use)++;
  if (*use == 64) {
    *use = 0;
    *acc = **mor;
    *mor = (*mor)+1;
  }

  if (isCell) {
    return frag_load_cell(tab, tabSz, use, acc, mor);
  }

  // `tmp` is the remaining bits from acc (high bits) combined
  // with the low bits of the next word.  We mask out the `refSz`
  // low bits from this to get the index into the backrefs table.

  u64 maxref = tabSz-1;
  u64 refSz = u64_bits(maxref);
  int remain = 64 - *use;
  u64 tmp = (remain==64) ? *acc : ((*acc >> *use) | (**mor << remain));// combine
  u64 ref = tmp & ((1ULL << refSz) - 1ULL);                            // mask

  // move forward by refSz bits.
  *use += refSz;
  if (*use >= 64) {
    *use -= 64;
    *acc = **mor;
    *mor = (*mor)+1;
  }

  return tab[ref];
}

Value * seed_load(u64 *buf) {
  u64 n_holes = buf[0];
  u64 n_bigs  = buf[1];
  u64 n_words = buf[2];
  u64 n_bytes = buf[3];
  u64 n_frags = buf[4];

  if (n_holes != 0) {
    fprintf(stderr, "file is just one seed, expected seedpod\n");
    exit(5);
  }

  u64 n_entries = n_bigs + n_words + n_bytes + n_frags;

  Value **tab = malloc(sizeof(Value*) * n_entries);

  // How big are the bignats?
  u64 bigwidths[n_bigs];
  for (int i=0; i<n_bigs; i++) {
    bigwidths[i] = buf[5+i];
  }

  Value **next_ref = tab;
  int used = 5 + n_bigs; // number of words used

  for (int i=0; i<n_bigs; i++) {
    u64 wid  = bigwidths[i];

    u64 * big_buf = calloc(wid, sizeof(u64));
    big_buf = memcpy(big_buf, buf+used, wid*sizeof(u64));
    Nat big_nat = (Nat){.type=BIG, .size=wid, .buf = big_buf};

    *next_ref++ = a_Big(big_nat);
    used += wid;
  }

  for (int i=0; i<n_words; i++) {
    *next_ref++ = a_Nat(buf[used++]);
  }

  {
    uint8_t *byte_buf = (void*) (buf + used);
    for (int i=0; i<n_bytes; i++) {
      *next_ref++ = a_Nat(byte_buf[i]);
    }
    used += (n_bytes / 8);
  }

  int use = 8 * (n_bytes%8);
  u64 acc = buf[used];
  u64 *more = &buf[used+1];

  for (int i=0; i<n_frags; i++) {
    u64 tabSz = (next_ref - tab);
    *next_ref++ = frag_load_cell(tab, tabSz, &use, &acc, &more);
  }

  return next_ref[-1];
}

u64 *load_seed_file (const char* filename, u64 *sizeOut) {
  FILE * f = fopen (filename, "rb");

  if (!f) exit(2);

  fseek(f, 0, SEEK_END);
  u64 szBytes = ftell(f);

  u64 szWords = (szBytes / 8) + (szBytes%8 ? 1 : 0);

  fseek(f, 0, SEEK_SET);
  u64 *buf = calloc(szWords+1, 8); // We add an extra word here
                                   // so that we can over-read
                                   // by one word, this simplifies
                                   // decoding.
  if (!buf) exit(3);
  if (fread (buf, 1, szBytes, f) != szBytes) exit(4);
  fclose(f);

  *sizeOut = szWords;
  return buf;
}

////////////////////////////////////////////////////////////////////////////////
//  Runner

void run(Value * v) {
  trace_print("RUN[%s]\n", print_value(v));
  trace_print("  ->\n", print_value(v));
  print_depth++;
  Value * res = F(v);
  print_depth--;
  trace_print("%s\n", print_value(res));
}

Value *read_atom() {
  char c;
  u64 acc = 0;
  while (isdigit(c = getchar())) {
    acc = acc*10 + (c - '0');
  }
  ungetc(c,stdin);
  return a_Nat(acc);
}

void eat_spaces() {
  char c;
  while (isspace(c = getchar()));
  ungetc(c, stdin);
}

Value *read_exp();

Value *read_app(Value *f) {
  while (true) {
    char c = getchar();
    switch (c) {
      case ' ':
        eat_spaces();
        c = getchar();
        if (c == ')') return f;
        ungetc(c, stdin);
        f = a_App(f,read_exp());
        continue;
      case ')':
        return f;
      default:
        crash("expecting space or )");
    }
  }
}

Value *read_sym() {
    static char buf[1024];
    char c, *out=buf;
    while (issym(c = getchar())) *out++ = c;
    if (feof(stdin)) crash("Unexpected EOF\n");
    ungetc(c, stdin);
    int len = strlen(buf);
    if (!len)    crash("Empty symbol");
    if (len > 8) crash("TODO: big symbols");
    u64 word = 0;
    strcpy((char*)&word, buf);
    return a_Nat(word);
}

Value *read_exp() {
  again:
    char c = getchar();
    if (!c) return NULL;
    switch (c) {
    case '%': {
        return read_sym();
    }
    case '<': {
        char buf[1234] = {0};
        for (int i=0; i<1234; i++) {
            buf[i] = getchar();
            if (feof(stdin)) {
                crash("Unexpected EOF");
            }
            if (buf[i] == '>') {
                buf[i] = 0;
                u64 seedSz;
                u64 *words = load_seed_file(buf, &seedSz);
                Value *loaded = seed_load(words);
                check_value(loaded);
                return loaded;
            }
        }
        crash("load files");
        break;
    }
    case '(': {
        eat_spaces();
        Value *f = read_exp();
        read_app(f);
        break;
    }
    default:
        if (isdigit(c)) {
            ungetc(c, stdin);
            return read_atom();
        }
        fprintf(stderr, "Unexpected: '%c'\n", c);
        exit(2);
    }
}

Value *read_exp_top() {
 again:
  eat_spaces();
  if (feof(stdin)) return NULL;
  return read_exp();
}

int main (void) {
  again:
    printf(">> ");
    Value *v = read_exp_top();
    if (!v) return 0;
    run(v);
    printf("%s\n", print_value(v));
    goto again;
    return 0;
}
