// function overloading
import std.stdio;

int foo(int i) {
    return i;
}

double foo(double i) {
    return i;
}


void main() {
    double almostPi = 3.14;
    int zero = 0;

    writeln("This time I print double: ", foo(almostPi));
    writeln("And this time int: ", foo(zero));
}


// coercion
import std.stdio;

double foo(double i) {
    return i;
}


void main() {
    double almostPi = 3.14;
    int zero = 0;

    writeln("This time I print double: ", foo(almostPi));
    writeln("And this time int: ", foo(zero));
}


// parametric / polytype
import std.stdio;

T returnGeneric(T)(T x) {
    return x;
}

void main() {
    writeln("Now I print int: ", returnGeneric!(int)(5));
    writeln("But I can also print any printable type," ~
            "such as arrays: ", returnGeneric!(int[])([1,2,3,4]));
}


// inclusion / inheritance
import std.stdio : writeln;

class A {
    // protected is just seen by inheriting classes
    protected string type;

    this(string type) {
        this.type = type;
    }

    abstract void print();
}

class B : A {
    private {
        int number;
    }

    // constructor
    this(int number) {
        // call base class constructor
        super("B");
        this.number = number;
    }

    override void print() {
        writeln("I'm a B object and my number is: ", number);
    }
}

class C : A {
    private {
        char c;
    }

    // constructor
    this(char c) {
        // call base class constructor
        super("C");
        this.c = c;
    }

    override void print() {
        writeln("I'm a C object any my char is: ", c);
    }
}

void main() {
    A[] anys = [
        new B(10),
        new C('D')
    ];

    foreach (any; anys) {
        any.print();
    }
}


// operator overloading
import std.stdio;

class S {
    int m;

    // constructor
    this(int m) { this.m = m; }

    // unary operator minus
    S opUnary(string s)() if (s == "-") {
        return new S(-m);
    }
}

void main() {
    S s = new S(5);
    S s_minus = -s;

    writeln("This is what s is: ", s.m);
    writeln("But with op- we get: ", s_minus.m);
}
