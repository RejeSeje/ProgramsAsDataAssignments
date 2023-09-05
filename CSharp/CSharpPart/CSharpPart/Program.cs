using System;
using System.Collections.Generic;
using System.Security.Permissions;

namespace CSharpPart
{
    abstract class Expr
    {
        abstract public int Eval(Dictionary<string, int> env);

        abstract public string ToString();
    }

    abstract class Binop : Expr
    {
        protected readonly Expr e1, e2;

        public Binop(Expr e1, Expr e2)
        {
            this.e1 = e1;
            this.e2 = e2;
        }

        abstract public Expr Simplify(Dictionary<string, int> env);
    }

    class CstI : Expr
    {
        protected readonly int i;

        public CstI(int i)
        {
            this.i = i;
        }

        public override int Eval(Dictionary<string, int> env)
        {
            return i;
        }

        public override string ToString()
        {
            return $"{i.ToString()}";
        }
    }

    class Var : Expr
    {
        protected readonly string a;

        public Var(string a)
        {
            this.a = a;
        }
        
        public override int Eval(Dictionary<string, int> env)
        {
            return env[a];
        }

        public override string ToString()
        {
            return $"{a}";
        }
    }

    class Add : Binop
    {
        public Add(Expr e1, Expr e2) : base(e1, e2) {}

        public override int Eval(Dictionary<string, int> env)
        {
            return e1.Eval(env) + e2.Eval(env);
        }

        public override string ToString()
        {
            return $"({e1.ToString()} + {e2.ToString()})";
        }

        public override Expr Simplify(Dictionary<string, int> env)
        {
            if (e1.Eval(env) == 0)
            {
                return e2;
            }
            if (e2.Eval(env) == 0)
            {
                return e1;
            }

            throw new Exception("not simplifiable");
        }
    }
    
    class Mul : Binop
    {
        public Mul(Expr e1, Expr e2) : base(e1, e2) {}

        public override int Eval(Dictionary<string, int> env)
        {
            return e1.Eval(env) * e2.Eval(env);
        }

        public override string ToString()
        {
            return $"({e1.ToString()} * {e2.ToString()})";
        }

        public override Expr Simplify(Dictionary<string, int> env)
        {
            if (e1.Eval(env) == 1)
            {
                return e2;
            }
            if (e2.Eval(env) == 1)
            {
                return e1;
            }

            if (e1.Eval(env) == 0 || e2.Eval(env) == 0)
            {
                return new CstI(0);
            }

            throw new Exception("not simplifiable");
        }
    }
    
    class Sub : Binop
    {
        public Sub(Expr e1, Expr e2) : base(e1, e2) {}
        
        public override int Eval(Dictionary<string, int> env)
        {
            return e1.Eval(env) - e2.Eval(env);
        }

        public override string ToString()
        {
            return $"({e1.ToString()} - {e2.ToString()})";
        }

        public override Expr Simplify(Dictionary<string, int> env)
        {
            if (e2.Eval(env) == 0)
            {
                return e1;
            }

            if (e1.Eval(env) == e2.Eval(env))
            {
                return new CstI(0);
            }

            throw new Exception("not simplifiable");
        }
    }
    
    internal class Program
    {
        public static void Main(string[] args)
        {
            //Exercise 1.4 (II)
            Expr e = new Add(new CstI(17), new Var("z"));

            Expr e1 = new Mul(new CstI(123), new Var("y"));

            Expr e2 = new Sub(new Mul(new Var("a"), new Var("b")), new CstI(1));

            Expr e3 = new Mul(new Add(new CstI(1), new CstI(2)), new Mul(new CstI(12), new CstI(10)));
            
            Console.WriteLine(e.ToString());
            Console.WriteLine(e1.ToString());
            Console.WriteLine(e2.ToString());
            Console.WriteLine(e3.ToString());
            
            //Exercise 1.4 (III)
            Dictionary<string, int> env = new Dictionary<string, int>();

            Console.WriteLine(e3.Eval(env));
            
            //Exercise 1.4 (IV)
            Binop e4 = new Add(new CstI(0), new CstI(1));
            
            Console.WriteLine(e4.Simplify(env).ToString());
        }
    }
}