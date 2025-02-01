#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"
#include "gpp.h"

void yyrestart ( FILE *input_file  );

char * cloneStr(char * s) {
    char * clone = malloc(strlen(s) + 1);
    strcpy(clone, s);
    return clone;
}

typedef struct {
    int size;
    int used;
    void ** data;
} vector;

vector * newVector(int size) {
    size = size < 1 ? 1 : size;
    vector * v = malloc(sizeof(vector));
    v->size = size;
    v->used = 0;
    v->data = malloc(sizeof(void*) * size);
    return v;
}

void freeVector(vector * v) {
    free(v->data);
    free(v);
}

void addVector(vector * v, void * p) {
    if(v->size == v->used) {
        v->size *= 2;
        v->data = realloc(v->data, sizeof(void*) * v->size);
    }
    v->data[v->used++] = p;
}

node * newNode(int type, data d) {
    node * n = malloc(sizeof(node));
    *n = (node){0};
    n->type = type;
    n->d = d;
    return n;
}

node * addSibling(node * n, node * s) {
    node ** i = &n;
    while(*i) i = &(*i)->sibling;
    *i = s;
    return n;
}

node * addChild(node * n, node * c) {
    n->child = addSibling(n->child, c);
    return n;
}

int countSiblings(node * n) {
    if(!n) return 0;
    int res;
    for(res = 0; n; n = n->sibling, ++res);
    return res;
}

data cloneData(data d) {
    data res = d;
    switch(d.type) {
        case STRING:
            res.ptr = cloneStr(d.ptr);
            break;
        case LIST: {
                vector * items = d.ptr;
                vector * cloneList = newVector(1);
                for(int i = 0; i < items->used; ++i) {
                    data * clone = malloc(sizeof(data));
                    addVector(cloneList, clone);
                    *clone = cloneData(*(data*)items->data[i]);
                }
                res.ptr = cloneList;
                break;
            }
    }
    return res;
}

void destroyData(data d) {
    switch(d.type) {
        case STRING:
            free(d.ptr);
            break;
        case LIST: {
                vector * items = d.ptr;
                for(int i = 0; i < items->used; ++i) {
                    data * item = items->data[i];
                    destroyData(*item);
                    free(item);
                }
                freeVector(items);
                break;
            }
    }
}

void destroyDataList(data * l, int n) {
    for(int i = 0; i < n; ++i) destroyData(l[i]);
    free(l);
}

void printData(data d) {
    switch(d.type) {
        case VALUEF:
            printf("%d:%d", d.fraction.top, d.fraction.bottom);
            break;
        case VALUEI:
            printf("%d", d.integer);
            break;
        case BOOLEAN:
            printf(d.boolean ? "TRUE" : "FALSE");
            break;
        case STRING:
            printf("\"%s\"", (char*)d.ptr);
            break;
        case LIST: {
                vector * items = d.ptr;
                if(items->used) {
                    printf("(");
                    printData(*(data*)items->data[0]);
                    for(int i = 1; i < items->used; ++i) {
                        printf(", ");
                        printData(*(data*)items->data[i]);
                    }
                    printf(")");
                }
                else {
                    printf("NIL");
                }
                break;
            }
        case FUNCTION:
            printf("FUNCTION");
            break;
        default:
            printf("UNKNOWN");
    }
}

vector * varStack = 0;

typedef struct {
    char * name;
    data d;
} variable;

void increaseVarStack() {
    if(!varStack) {
        varStack = newVector(1);
    }
    addVector(varStack, newVector(1));
}

void decreaseVarStack() {
    if(!varStack || varStack->used == 0) return;
    vector * cur = varStack->data[--varStack->used];
    for(int i = 0; i < cur->used; ++i) {
        variable * v = cur->data[i];
        destroyData(v->d);
        free(v);
    }
    freeVector(cur);
    if(varStack->used == 0) {
        freeVector(varStack);
        varStack = 0;
    }
}

variable * getVar(char * name) {
    for(int i = varStack->used - 1; i >= 0; --i) {
        vector * cur = varStack->data[i];
        for(int j = 0; j < cur->used; ++j) {
            variable * var = (variable*)cur->data[j];
            if(!strcmp(name, var->name)) {
                return var;
            }
        }
    }
    return 0;
}

variable * defVar(char * name, data d) {
    vector * cur = varStack->data[varStack->used - 1];
    for(int i = 0; i < cur->used; ++i) {
        variable * var = (variable*)cur->data[i];
        if(!strcmp(name, var->name)) {
            printf("ERROR: Variable %s is already defined\n", name);
            return 0;
        }
    }
    variable * v = malloc(sizeof(variable));
    v->name = name;
    v->d = d;
    addVector(varStack->data[varStack->used - 1], v);
    return v;
}

int gcd(int a, int b) {
    if(a < b) {
        int t = a;
        a = b;
        b = t;
    }
    if(b == 0) return a;
    return gcd(a % b, b);
}

frac simplify(frac f) {
    int sign = 1;
    if(f.top < 0) {
        f.top *= -1;
        sign *= -1;
    }
    if(f.bottom < 0) {
        f.bottom *= -1;
        sign *= -1;
    }
    int g = gcd(f.top, f.bottom);
    f.top /= g;
    f.bottom /= g;
    f.top *= sign;
    return f;
}

frac addFrac(frac a, frac b) {
    return simplify((frac){a.top * b.bottom + b.top * a.bottom, a.bottom * b.bottom});
}

frac subFrac(frac a, frac b) {
    return simplify((frac){a.top * b.bottom - b.top * a.bottom, a.bottom * b.bottom});
}

frac mulFrac(frac a, frac b) {
    return simplify((frac){a.top * b.top, a.bottom * b.bottom});
}

frac divFrac(frac a, frac b) {
    return simplify((frac){a.top * b.bottom, a.bottom * b.top});
}

int eqFrac(frac a, frac b) {
    return a.top * b.bottom == b.top * a.bottom;
}

int lessFrac(frac a, frac b) {
    return a.top * b.bottom < b.top * a.bottom;
}

int fracToInt(frac f) {
    return f.top / f.bottom;
}

data cast(int type, data d) {
    if(type == d.type) return cloneData(d);
    data res = {};
    if(type == VALUEF) {
        if(d.type == VALUEI)
            res = (data){.type = VALUEF, .fraction = (frac){.top = d.integer, .bottom = 1}};
    }
    else if(type == VALUEI) {
        if(d.type == VALUEF)
            res = (data){.type = VALUEI, .integer = fracToInt(d.fraction)};
    }
    else if(type == BOOLEAN) {
        res = (data){.type = BOOLEAN, .boolean = !(d.type == LIST && ((vector*)d.ptr)->used == 0)};
    }
    return res;
}

data * evalArguments(node * explist, int * checks, int * n) {
    int nExp = countSiblings(explist->child);
    data * res = nExp ? malloc(nExp * sizeof(data)) : 0;
    int ri = 0;
    node * cur = explist->child;
    for(struct {int type, count;} * check = (void*)checks; check->type; ++check) {
        for(int i = 0; (check->count == -1 && cur) || i < check->count; ++i) {
            data d = eval(cur);
            if(!d.type) {
                printf("ERROR: Failed to evaluate an argument\n");
                *n = -1;
                destroyData(d);
                destroyDataList(res, ri);
                return 0;
            }
            cur = cur->sibling;
            if(check->type != -1) {
                data tmp = d;
                d = cast(check->type, d);
                destroyData(tmp);
            }
            if(!d.type) {
                printf("ERROR: Failed to cast\n");
                *n = -1;
                destroyDataList(res, ri);
                return 0;
            }
            res[ri++] = d;
        }
    }
    if(ri != nExp) {
        printf("ERROR: Invalid number of arguments\n");
        *n = -1;
        destroyDataList(res, ri);
        return 0;
    }
    if(n) *n = nExp;
    return res;
}

data eval(node * n) {
    data res = {};
    if(gppstop || !n) return res;
    data * args = 0;
    int argc;
    switch(n->type) {
        case VALUEF:
        case VALUEI:
        case BOOLEAN:
        case STRING:
            res = cloneData(n->d);
            break;
        case KW_NIL:
            res = (data){.type = LIST, .ptr = newVector(1)};
            break;
        case LIST: {
                vector * v = newVector(1);
                res = (data){.type = LIST, .ptr = v};
                for(node * cur = n->child; cur; cur = cur->sibling) {
                    data * dp = malloc(sizeof(data));
                    *dp = eval(cur);
                    if(!dp->type) {
                        destroyData(res);
                        res = (data){};
                        break;
                    }
                    addVector(v, dp);
                }
                break;
            }
        case IDENTIFIER: {
                variable * v = getVar(n->d.ptr);
                if(!v) {
                    printf("ERROR: Undefined variable %s\n", (char*)n->d.ptr);
                    break;
                }
                res = cloneData(v->d);
                break;
            }
        case OP_PLUS: {
                args = evalArguments(n->child, (int[]){VALUEF, 1, VALUEF, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                frac sum = args[0].fraction;
                for(int i = 1; i < argc; ++i) {
                    sum = addFrac(sum, args[i].fraction);
                }
                res = (data){.type = VALUEF, .fraction = sum};
                break;
            }
        case OP_MINUS: {
                args = evalArguments(n->child, (int[]){VALUEF, 1, VALUEF, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                frac sum = args[0].fraction;
                for(int i = 1; i < argc; ++i) {
                    sum = subFrac(sum, args[i].fraction);
                }
                res = (data){.type = VALUEF, .fraction = sum};
                break;
            }
        case OP_MULT: {
                args = evalArguments(n->child, (int[]){VALUEF, 1, VALUEF, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                frac sum = args[0].fraction;
                for(int i = 1; i < argc; ++i) {
                    sum = mulFrac(sum, args[i].fraction);
                }
                res = (data){.type = VALUEF, .fraction = sum};
                break;
            }
        case OP_DIV: {
                args = evalArguments(n->child, (int[]){VALUEF, 1, VALUEF, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                frac sum = args[0].fraction;
                for(int i = 1; i < argc; ++i) {
                    sum = divFrac(sum, args[i].fraction);
                }
                if(sum.bottom == 0) {
                    printf("ERROR: Division by zero\n");
                    break;
                }
                res = (data){.type = VALUEF, .fraction = sum};
                break;
            }
        case KW_AND: {
                args = evalArguments(n->child, (int[]){BOOLEAN, 1, BOOLEAN, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                int sum = 1;
                for(int i = 0; sum && i < argc; ++i) {
                    sum = sum && args[i].boolean;
                }
                res = (data){.type = BOOLEAN, .boolean = sum};
                break;
            }
        case KW_OR: {
                args = evalArguments(n->child, (int[]){BOOLEAN, 1, BOOLEAN, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                int sum = 0;
                for(int i = 0; !sum && i < argc; ++i) {
                    sum = sum || args[i].boolean;
                }
                res = (data){.type = BOOLEAN, .boolean = sum};
                break;
            }
        case KW_NOT: {
                args = evalArguments(n->child, (int[]){BOOLEAN, 1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                res = (data){.type = BOOLEAN, .boolean = !args[0].boolean};
                break;
            }
        case KW_EQUAL: {
                if(countSiblings(n->child->child) != 2) {
                    printf("ERROR: Invalid number of arguments\n");
                    break;
                }
                int isEqual = 0;
                data left = eval(n->child->child);
                data right = eval(n->child->child->sibling);
                if(left.type != right.type) {
                    data casted = cast(left.type, right);
                    if(!casted.type) {
                        casted = cast(right.type, left);
                        if(!casted.type) {
                            isEqual = 0;
                            destroyData(casted);
                        }
                        else {
                            destroyData(left);
                            left = casted;
                        }
                    }
                    else {
                        destroyData(right);
                        right = casted;
                    }
                }
                if(left.type == right.type) {
                    switch(left.type) {
                        case VALUEF:
                            isEqual = eqFrac(left.fraction, right.fraction);
                            break;
                        case VALUEI:
                            isEqual = left.integer == right.integer;
                            break;
                        case BOOLEAN:
                            isEqual = left.boolean && right.boolean || !(left.boolean || right.boolean);
                            break;
                        case STRING:
                            isEqual = strcmp(left.ptr, right.ptr);
                            break;
                        case LIST:
                            printf("\nTODO list comparison\n");
                            break;
                        default:
                            printf("ERROR: Unknown type\n");
                    }
                }
                destroyData(left);
                destroyData(right);
                res = (data){.type = BOOLEAN, .boolean = isEqual};
                break;
            }
        case KW_LESS: {
                args = evalArguments(n->child, (int[]){VALUEF, 2, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                res = (data){.type = BOOLEAN, .boolean = lessFrac(args[0].fraction, args[1].fraction)};
                break;
            }
        case KW_LIST: {
                vector * v = newVector(1);
                res = (data){.type = LIST, .ptr = v};
                for(node * cur = n->child->child; cur; cur = cur->sibling) {
                    data * dp = malloc(sizeof(data));
                    *dp = eval(cur);
                    if(!dp->type) {
                        destroyData(res);
                        res = (data){};
                        break;
                    }
                    addVector(v, dp);
                }
                break;
            }
        case KW_APPEND: {
                args = evalArguments(n->child, (int[]){LIST, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                res = (data){.type = LIST, .ptr = newVector(1)};
                for(int i = 0; i < argc; ++i) {
                    vector * cur = args[i].ptr;
                    for(int j = 0; j < cur->used; ++j) {
                        data * clone = malloc(sizeof(data));
                        data * d = cur->data[j];
                        *clone = cloneData(*d);
                        addVector(res.ptr, clone);
                    }
                }
                break;
            }
        case KW_CONCAT: {
                args = evalArguments(n->child, (int[]){STRING, -1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                int len = strlen(args[0].ptr);
                for(int i = 1; i < argc; ++i) {
                    len += strlen(args[i].ptr);
                }
                char * str = malloc(len + 1);
                str[0] = 0;
                for(int i = 0; i < argc; ++i) {
                    strcat(str, args[i].ptr);
                }
                res = (data){.type = STRING, .ptr = str};
                break;
            }
        case KW_EXIT: {
                gppstop = 1;
                res = (data){.type = VALUEI, .integer = 0};
                break;
            }
        case KW_LOAD: {
                args = evalArguments(n->child, (int[]){STRING, 1, 0}, &argc);
                if(argc == -1) {
                    break;
                }
                FILE * fp = fopen(args[0].ptr, "r");
                if(fp) {
                    node * tree = 0;
                    yyrestart(fp);
                    yyparse(&res, &tree);
                    yyrestart(stdin);
                    addChild(n, tree);
                    fclose(fp);
                }
                else {
                    printf("ERROR: File %s not found", (char*)args[0].ptr);
                }
                break;
            }
        case KW_DISP: {
                if(countSiblings(n->child->child) != 1) {
                    printf("ERROR: Invalid number of arguments\n");
                    break;
                }
                data e = eval(n->child->child);
                printData(e);
                printf("\n");
                res = e;
                break;
            }
        case KW_DEFFUN: {
                variable * v = defVar(n->child->d.ptr, (data){.type = FUNCTION, .ptr =  n->child->sibling});
                if(v) res = cloneData(v->d);
                break;
            }
        case KW_DEFVAR: {
                if(countSiblings(n->child->sibling->child) != 1) {
                    printf("ERROR: Invalid number of arguments\n");
                    break;
                }
                data e = eval(n->child->sibling->child);
                variable * v = defVar(n->child->d.ptr, e);
                if(v) res = cloneData(v->d);
                break;
            }
        case KW_SET: {
                if(countSiblings(n->child->sibling->child) != 1) {
                    printf("ERROR: Invalid number of arguments\n");
                    break;
                }
                variable * var = getVar(n->child->d.ptr);
                if(!var) {
                    printf("ERROR: Variable %s is not defined\n", (char*)n->child->d.ptr);
                    break;
                }
                data ev = eval(n->child->sibling->child);
                data e = cast(var->d.type, ev);
                destroyData(ev);
                if(!e.type) {
                    printf("ERROR: Casting failure in variable assignment\n");
                    destroyData(e);
                    break;
                }
                destroyData(var->d);
                var->d = e;
                res = cloneData(e);
                break;
            }
        case CALL: {
                data func = eval(n->child);
                if(func.type != FUNCTION) {
                    printf("ERROR: Given expression is not a function\n");
                    destroyData(func);
                    break;
                }
                node * idlist = ((node*)func.ptr)->child;
                node * arglist = n->child->sibling->child;
                if(countSiblings(idlist) != countSiblings(arglist)) {
                    printf("ERROR: Invalid number of arguments\n");
                    break;
                }
                increaseVarStack();
                for(node * curi = idlist, * cura = arglist; curi && cura; curi = curi->sibling, cura = cura->sibling) {
                    defVar(curi->d.ptr, eval(cura));
                }
                res = eval(((node*)func.ptr)->sibling);
                decreaseVarStack();
                break;
            }
        case EXPLIST: {
                res = (data){.type = LIST, .ptr = newVector(1)};
                for(node * cur = n->child; cur; cur = cur->sibling) {
                    destroyData(res);
                    res = eval(cur);
                    if(!res.type) break;
                }
                break;
            }
        case KW_IF: {
                int nExp = countSiblings(n->child->sibling->child);
                if(nExp > 2 || nExp < 1) {
                    printf("ERROR: Invalid number of arguments\n");
                    break;
                }
                data ev = eval(n->child);
                data cond = cast(BOOLEAN, ev);
                destroyData(ev);
                if(cond.boolean) {
                    res = eval(n->child->sibling->child);
                }
                else if(nExp == 2) {
                    res = eval(n->child->sibling->child->sibling);
                }
                else {
                    res = (data){.type = LIST, .ptr = newVector(1)};
                }
                break;
            }
        case KW_WHILE: {
                res = (data){.type = LIST, .ptr = newVector(1)};
                increaseVarStack();
                while(cast(BOOLEAN, eval(n->child)).boolean) {
                    destroyData(res);
                    res = eval(n->child->sibling);
                    if(!res.type) break;
                }
                decreaseVarStack();
                break;
            }
        case KW_FOR: {
                res = (data){.type = LIST, .ptr = newVector(1)};
                increaseVarStack();
                data ev = eval(n->child->sibling);
                defVar(n->child->d.ptr, cast(VALUEI, ev));
                destroyData(ev);
                ev = eval(n->child->sibling->sibling);
                int final = cast(VALUEI, ev).integer;
                destroyData(ev);
                variable * i = getVar(n->child->d.ptr);
                while(i->d.integer < final) {
                    destroyData(res);
                    res = eval(n->child->sibling->sibling->sibling);
                    if(!res.type) break;
                    i->d.integer++;
                }
                decreaseVarStack();
                break;
            }
    }
    if(args) destroyDataList(args, argc);
    return res;
}

int gppstop = 0;

int yylex_destroy();

void destroyTree(node * t) {
    if(!t) return;
    destroyTree(t->sibling);
    destroyTree(t->child);
    destroyData(t->d);
    free(t);
}

int main() {
    data final = {};
    node * tree = 0;
    increaseVarStack();
    yyparse(&final, &tree);
    decreaseVarStack();
    destroyTree(tree);
    destroyData(final);
    yylex_destroy();
    return 0;
}
