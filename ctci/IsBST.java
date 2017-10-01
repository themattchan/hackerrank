import java.util.function.*;

class IsBST {
    class Node {
        int data;
        Node left;
        Node right;
    }

    boolean checkBST(Node root) {
        if (root == null) return true;

        Any l = foldMap(n -> new Any(n.data < root.data), root.left).getMonoid();
        Any r = foldMap(n -> new Any(n.data > root.data), root.right).getMonoid();
        return l.mappend(r).getAny() && checkBST(root.left) && checkBST(root.right);
    }

    interface Monoid<T> {
        T mempty();
        T mappend(T that);
        Monoid<T> mappend(Monoid<T> that);
        T getMonoid();
    }
    class Any implements Monoid<Any> {
        boolean b;
        Any() {
            this.b = false;
        }
        Any(boolean b) {
            this.b = b;
        }
        public boolean getAny() {
            return b;
        }
        public Any mempty() {
            return new Any();
        }
        public Any mappend(Any that) {
            return new Any(this.getAny() || that.getAny());
        }
        public Monoid<Any> mappend(Monoid<Any> that) {
            return this.mappend(that.getMonoid());
        }
        public Any getMonoid() {
            return this;
        }
    }

    <T> Monoid<T> foldMap(Function<Node,Monoid<T>> f, Node root) {
        Monoid<T> t = f.apply(root);
        Monoid<T> l = foldMap(f, root.left);
        Monoid<T> r = foldMap(f, root.right);
        return t.mappend(l).mappend(r);
    }
}
