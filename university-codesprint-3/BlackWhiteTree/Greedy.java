import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;
import java.util.regex.*;

@SuppressWarnings("unchecked")
public class Solution {
    final boolean DEBUG = false;

    int NODES;
    BitSet COLORS;
    ArrayList<Integer>[] GRAPH; // this is *very* sparse

    int STRANGENESS;
    int BLACKS;
    int WHITES;
    int SIZE;
    int CUTS;

    public Solution(int nodes, BitSet colors, ArrayList<Integer>[] graph) {
        this.NODES = nodes;
        this.COLORS = colors;
        this.GRAPH = graph;
        this.SIZE = this.NODES;
        this.CUTS = 0;

        updateState();
        if (DEBUG) assert(checkInvariants());
    }

    public static void run(Scanner in) {
        int nodes = in.nextInt();
        BitSet colors = new BitSet(nodes+1);
        for (int c_i = 1; c_i <= nodes; c_i++){
            int i = in.nextInt();
            if (i == 1) //black node
                colors.set(c_i);
        }
        ArrayList<Integer>[] graph = new ArrayList[nodes+1];
        for (int i = 0; i <= nodes; i++) {
            graph[i] = new ArrayList<>();
        }
        for (int i = 0; i < nodes -1; i++){
            int x = in.nextInt();
            int y = in.nextInt();
            graph[x].add(y);
            graph[y].add(x);
        }
        Solution sol = new Solution(nodes,colors,graph);
        sol.solve();
        sol.printSolution();
        in.close();
    }

    public static void main(String[] args) throws Exception {
        if (args.length > 0) {
            run(new Scanner(new File(args[0])));
        } else {
            run(new Scanner(System.in));
        }
    }

    int strangeness(int black, int white) {
        return Math.abs(black - white);
    }

    boolean isblack(int i) {
        return COLORS.get(i);
    }

    boolean isnode(int i) {
        return GRAPH[i] != null;
    }
    boolean iscut(int i) {
        return GRAPH[i] == null;
    }

    boolean isleaf(int i) {
        return isnode(i) && GRAPH[i].size() == 1;
    }

    void updateState() {
        int blacks = 0;
        int whites = 0;
        for (int i = 1; i <= NODES; i++) {
            if (! iscut(i)) {
                if (isblack(i))
                    blacks++;
                else
                    whites++;
            }
        }
        this.BLACKS = blacks;
        this.WHITES = whites;
        this.STRANGENESS = strangeness(blacks,whites);
        assert (blacks + whites == this.SIZE);
    }

    void printSolution() {
        System.out.println(STRANGENESS);
        System.out.println(SIZE);
        for (int i = 1; i <= NODES; i++) {
            if (isnode(i)) {
                System.out.print(i + " ");
            }
        }
        System.out.println("");
    }

    boolean checkInvariants() {
        int nodes = 0;
        int cuts = 0;
        for (int i = 1; i <= NODES; i++) {
            if (isnode(i)) {
                nodes++;
            } else {
                cuts++;
                // also check for dangling links...
                for (int j = 1; j <= NODES; i++) {
                    // j -> i
                    if (GRAPH[j].contains(i)) {
                        return false;
                    }
                }
            }
        }
        return
            (this.SIZE + this.CUTS == this.NODES) &&
            (this.BLACKS + this.WHITES == this.SIZE) &&
            (nodes == this.SIZE) &&
            (cuts == this.CUTS);

    }
    // mutates explored
    void colorchain(HashSet explored, int i) {
        if (! explored.contains(i)) {
            boolean color = isblack(i);
            int up = GRAPH[i].get(0);

            // dfs on the parent of i
            Stack<Integer> stk = new Stack<>();
            stk.push(i);
            while (! stk.isEmpty()) {
                int v = stk.pop();
                if (! explored.contains(v) && GRAPH[v].size() <= 2) {
                    explored.add(v);
                    // explore it
                    for (int w : GRAPH[v]) {
                        if (isnode(w) && isblack(w) == color)
                            stk.push(w);
                    }
                }
            }
        }
    }

    // O(NM)
    void sweep(HashSet<Integer> rm) {
        for (int i = 1; i <= NODES; i++) {
            if (rm.contains(i)) {
                GRAPH[i] = null;
                this.SIZE--;
                this.CUTS++;
            }
            else if (GRAPH[i] != null) {
                Iterator<Integer> adjIt = GRAPH[i].iterator();
                while (adjIt.hasNext()) {
                    int adj = adjIt.next();
                    if (rm.contains(i) || GRAPH[adj] == null) {
                        adjIt.remove();
                    }
                }
            }
        }
    }
    // first, find the most popular colour in the tree. we will maximise this
    // colour by cutting the other.
    // tally both colours
    // cut all leaves.
    void solve() {
        while (true) {
            HashSet<Integer> black = new HashSet<>();
            HashSet<Integer> white = new HashSet<>();

            for (int i = 1; i <= NODES; i++) {
                if (isleaf(i)) {
                    if (isblack(i)) {
                        colorchain(black, i);
                    } else {
                        colorchain(white, i);
                    }
                }
            }

            int strangeness_if_black = strangeness(BLACKS - black.size(), WHITES);
            int strangeness_if_white = strangeness(BLACKS, WHITES - white.size());

            boolean cut_black = strangeness_if_black > strangeness_if_white;
            int new_strange = (cut_black ? strangeness_if_black : strangeness_if_white);
            boolean do_cut = this.STRANGENESS < new_strange;

            if (! do_cut) return;

            if (cut_black)  {
                sweep(black);
                this.BLACKS -= black.size();
            } else {
                sweep(white);
                this.WHITES -= white.size();
            }
            this.STRANGENESS = new_strange;
        }
    }
}
