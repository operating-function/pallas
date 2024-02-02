#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>

typedef uint64_t U64;

typedef enum Type {
  PIN,
  LAW,
  APP,
  NAT,
  HOL
} Type;

typedef U64 Nat;
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

static const tracing = true;

int print_depth = 0;

void trace_print(const char * format, ...) {
  int d = print_depth*4;
  if (tracing) {
    for (int i=0; i<d; i++) putchar(' ');
    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
  }
}

Value * a_Nat(Nat n) {
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

void crash(char * s) {
  printf("Error: %s\n", s);
  exit(1);
}

char * print_value(Value * v) {
  char * buf = malloc(512*sizeof(char));
  print_value_internal(v, buf, 0);
  return buf;
}

void print_value_internal(Value * v, char * buf, int recur) {
  if (recur > 10) {
      sprintf(buf + strlen(buf), "..");
      return;
  }

  switch (v->type) {
    case PIN:
      sprintf(buf + strlen(buf), "<");
      print_value_internal(v->p, buf, recur+1);
      sprintf(buf + strlen(buf), ">");
      break;
    case LAW:
      sprintf(buf + strlen(buf), "{%llu %llu ", v->l.n, v->l.a);
      print_value_internal(v->l.b, buf, recur+1);
      sprintf(buf + strlen(buf), "}");
      break;
    case APP:
      sprintf(buf + strlen(buf), "(");
      print_value_internal(v->a.f, buf, recur+1);
      sprintf(buf + strlen(buf), " ");
      print_value_internal(v->a.g, buf, recur+1);
      sprintf(buf + strlen(buf), ")");
      break;
    case NAT:
      sprintf(buf + strlen(buf), "%llu", v->n);
      break;
    case HOL:
      sprintf(buf + strlen(buf), "<>");
      break;
  }
}

Value * F(Value * o);
Value * E(Value * o);
Value * L(Value * n, Value * e, Value * v, Value * b);
Value * R(Value * n, Value * e, Value * b);

Value * I(Value * f, Value * e, Value * n) {
  trace_print("I[%s, %s, %s]\n", print_value(f), print_value(e), print_value(n));

  if (n->n == 0 && e->type == APP) {
    return e->a.g;
  } else if (n->n == 0 && e->type != APP) {
    return e;
  } else if (n->n != 0 && e->type == APP) {
    return I(f, e->a.f, a_Nat(n->n-1));
  } else if (n->n != 0 && e->type != APP) {
    return f;
  }
}

Value * A_(Value * o) {
  switch (o->type) {
    case APP:
      Value * head = A_(o->a.f);
      if (head->n == 0) return a_Nat(0);
      return a_Nat(head->n - 1);
    case PIN:
      return A_(o->p);
    case LAW:
      return a_Nat(o->l.a);
    case NAT:
      Value * res;
      switch (o->n) {
        case 0:   res = a_Nat(3); break;
        case 1:   res = a_Nat(5); break;
        case 2:   res = a_Nat(3); break;
        default:  res = a_Nat(1); break;
      }
      return res;
    case HOL:
      crash("<<loop>>");
  }
}

Value * A(Value * o) {
  char * os = print_value(o);
  trace_print("A[%s]", print_value(o));
  Value *res = A_(o);
  if (tracing) printf(" ==> %d\n", res->n);
  return res;
}

Value * N(Value * o) {
  trace_print("N[%s]\n", print_value(o));
  Value * norm = E(o);
  if (norm->type == NAT) return norm;
  return a_Nat(0);
}

Value * L(Value * n, Value * e, Value * v, Value * b) {
  trace_print("L[%s, %s, %s, %s]\n", print_value(n), print_value(e), print_value(v), print_value(b));
  Value * x = a_Hol();
  Value * f = a_App(e, x);
  *x = *R(a_Nat((n->n)+1), f, v);
  return R(a_Nat((n->n)+1), f, b);
}

Value * R(Value * n, Value * e, Value * b){
  trace_print("R[%s, %s, %s]\n", print_value(n), print_value(e), print_value(b));
  if (b->type == NAT && (b->n <= n->n)) {
    return I(b, e, a_Nat(n->n - b->n));
  }
  if (b->type == APP) {
    if (b->a.f->type == APP) {
      if ((b->a.f->a.f->type == NAT) && (b->a.f->a.f->n == 0)) {
        Value * f = b->a.f->a.g;
        Value * x = b->a.g;
        return a_App(R(n, e, f), R(n, e, x));
      } else if ((b->a.f->a.f->type == NAT) && (b->a.f->a.f->n == 1)) {
        Value * f = b->a.f->a.g;
        Value * x = b->a.g;
        return L(n, e, f, x);
      }
    } else if ((b->a.f->type == NAT) && (b->a.f->n == 2)) {
        Value * x = b->a.g;
        return x;
    }
  }
  return b;
}

Value * C(Value * z, Value * p, Value * n) {
  trace_print("C[%s, %s, %s]\n", print_value(z), print_value(p), print_value(n));
  if (n->n == 0) {
    return z;
  } else {
    return a_App(p, a_Nat((n->n) - 1));
  }
}

Value * P(Value * p, Value * l, Value * a, Value * n, Value * o) {
  trace_print("P[%s, %s, %s, %s, %s]\n", print_value(p), print_value(l), print_value(a), print_value(n), print_value(o));
  switch (o->type) {
    case APP:
      return a_App(a_App(a, o->a.f), o->a.g);
    case PIN:
      return a_App(p, o->p);
    case LAW:
      return a_App(a_App(a_App(l, a_Nat(o->l.n)), a_Nat(o->l.a)), o->l.b);
    case NAT:
      return a_App(n, o);
    case HOL:
      crash("<<loop>>");
  }
}

Value * S(Value * o) {
  trace_print("S[%s]\n", print_value(o));
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
  trace_print("X[%s]\n", print_value(k));

  switch (k->type) {
    case APP:
      return X(k->a.f, e);
    case PIN:
      return X(k->p, e);
  }

  trace_print("    env: %s\n", print_value(e));

  switch (k->type) {
    case LAW:
      return R(a_Nat(k->l.a), e, k->l.b);
    case HOL:
      crash("<<loop>>");
  }

  Value *b, *a, *n, *p, *l, *x, *z;
  switch (k->n) {
    case 0:
      b = e->a.g;
      a = e->a.f->a.g;
      n = e->a.f->a.f->a.g;
      return a_Law(N(n)->n, N(a)->n, F(b));
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
      return a_Nat(N(x)->n + 1);
    case 4:
      x = e->a.g;
      return a_Pin(F(x));
  }
  crash(print_value(e));
}

Value * E(Value * o) {
  trace_print("E[%s]\n", print_value(o));
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
      print_depth--;
      if (arity->n == 1) {
        print_depth++;
        *o = *S(o);
        trace_print("S[] ==> %s\n", print_value(o));
        *o = *X(o,o);
        print_depth--;
        trace_print("E[] ==> %s\n", print_value(o));
        E(o);
      }
      return o;
    case LAW:
      print_depth++;
      if (o->l.a > 0) { return o; }
      Value *b = o->l.b;
      o->type = HOL;
      Value o1 = *R(a_Nat(0), o, b);
      *o = o1;
      print_depth--;
      return E(o);
  }
}

Value * F(Value * o) {
  trace_print("F[%s]\n", print_value(o));
  o = E(o);
  if (o->type == APP) {
    F(o->a.f);
    F(o->a.g);
  }
  return o;
}

void run(Value * v) {
  printf("RUN[%s]\n", print_value(v));
  print_depth++;
  Value * res = F(v);
  print_depth--;
  printf("%s\n", print_value(res));
}

int main(char argc, char ** argv) {

  Value * cnst = a_App(a_App(a_App(a_Nat(0), a_Nat(0)), a_Nat(2)), a_Nat(1));
  Value * appHead = a_App(a_App(a_App(a_App(a_Nat(1), a_Nat(0)), a_Nat(0)), cnst), a_Nat(0));

  Value * inc = a_App(a_Nat(3), a_Nat(4));
  Value * law = a_App(a_App(a_App(a_Nat(0), a_Nat(1)), a_Nat(2)), a_Nat(3));
  Value * pin = a_App(a_Nat(4), a_App(a_Nat(3), a_Nat(4)));
  Value * tn9 = a_App(a_App(a_App(a_Nat(2), a_Nat(0)), a_Nat(3)), a_Nat(9));
  Value * tn0 = a_App(a_App(a_App(a_Nat(2), a_Nat(0)), a_Nat(3)), a_App(a_Nat(4), a_Nat(9)));

  Value * pm1 = a_App(a_App(a_App(a_App(a_App(a_Nat(1), a_Nat(1)), a_Nat(0)), a_Nat(0)), a_Nat(0)), a_App(a_Nat(4), a_Nat(2)));
  Value * pm2 = a_App(a_App(a_App(a_App(a_App(a_Nat(1), a_Nat(0)), a_Nat(1)), a_Nat(0)), a_Nat(0)),
      a_App(a_App(a_App(a_Nat(0), a_Nat(2)), a_Nat(3)), a_Nat(4)));
  Value * pm3 = a_App(a_App(a_App(a_App(a_App(a_Nat(1), a_Nat(0)), a_Nat(0)), a_Nat(1)), a_Nat(0)), a_App(a_Nat(2), a_Nat(3)));
  Value * pm4 = a_App(a_App(a_App(a_App(a_App(a_Nat(1), a_Nat(0)), a_Nat(0)), a_Nat(0)), a_Nat(1)), a_Nat(2));

  Value * za = a_App(a_App(a_App(a_Nat(0), a_Nat(0)), a_Nat(0)), a_Nat(1));
  Value * fp = a_App(appHead, a_App(a_App(a_App(a_Nat(0), a_Nat(0)), a_Nat(0)), a_App(a_App(a_Nat(0), a_Nat(1)), a_Nat(0))));

  Value * rp1 = a_App(a_App(a_App(a_App(a_Nat(4), a_Nat(0)), a_Nat(1)), a_Nat(2)), a_Nat(0));
  Value * rp2 = a_App(a_App(a_App(a_Nat(4), a_App(a_Nat(0), a_Nat(1))), a_Nat(2)), a_Nat(0));
  Value * rp3 = a_App(a_App(a_App(a_Nat(4), a_App(a_App(a_App(a_Nat(0), a_Nat(1)), a_Nat(2)), a_Nat(0))), a_Nat(3)), a_Nat(4));
  Value * rp4 = a_App(a_App(a_App(a_Nat(4), a_App(a_Nat(4), a_App(a_App(a_App(a_Nat(0), a_Nat(1)), a_Nat(2)), a_Nat(0)))), a_Nat(3)), a_Nat(4));

//  run(inc);
//  run(law);
//  run(pin);
//  run(tn9);
//  run(tn0);
//  run(pm1);
//  run(pm2);
//  run(pm3);
//  run(pm4);
//  run(za);
  run(fp);
//  run(rp1);
//  run(rp2);
//  run(rp3);
//  run(rp4);

}
