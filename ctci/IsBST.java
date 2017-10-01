import java.util.function.*;

class IsBST {
    class Node {
        int data;
        Node left;
        Node right;
    }

    boolean checkBST(Node root) {
        if (root == null) return true;

        All l = foldMap(n -> n == null ? new All() : new All(n.data < root.data), root.left ).getMonoid();
        All r = foldMap(n -> n == null ? new All() : new All(n.data > root.data), root.right).getMonoid();
        return l.mappend(r).getAll() && checkBST(root.left) && checkBST(root.right);
    }

    interface Monoid<T> {
        T mempty();
        T mappend(T that);
        Monoid<T> mappend(Monoid<T> that);
        T getMonoid();
    }
    class All implements Monoid<All> {
        boolean b;
        All() {
            this.b = false;
        }
        All(boolean b) {
            this.b = b;
        }
        public boolean getAll() {
            return b;
        }
        public All mempty() {
            return new All();
        }
        public All mappend(All that) {
            return new All(this.getAll() && that.getAll());
        }
        public Monoid<All> mappend(Monoid<All> that) {
            return this.mappend(that.getMonoid());
        }
        public All getMonoid() {
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
