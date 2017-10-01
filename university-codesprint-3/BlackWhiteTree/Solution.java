import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;
import java.util.regex.*;

@SuppressWarnings("unchecked")
public class Solution {
    final boolean DEBUG = false;
    final int ROOT = 1;
    int NODES;
    BitSet COLORS;
    int[][] GRAPH; // this is *very* sparse
    int[] revorder;

    int STRANGENESS;
    int BLACKS;
    int WHITES;
    int SIZE;
    int CUTS;

    public Solution(int nodes, BitSet colors, ArrayList<Integer>[] graph) {
        this.NODES = nodes;
        this.COLORS = colors;
        this.SIZE = this.NODES;
        this.GRAPH = rooted(graph,ROOT);
        this.revorder = topo();
    }

    void topovisit(int n, Deque<Integer> reversed, BitSet seen) {
        if (seen.get(n)) return;
        for (int m : GRAPH[n]) {
            topovisit(m, reversed, seen);
        }
        seen.set(n);
        reversed.addLast(n);
    }
    int[] topo() {
        BitSet seen = new BitSet(NODES);
        ArrayDeque<Integer> reversed = new ArrayDeque<>();
        while (seen.cardinality() != NODES) {
            int n = seen.nextClearBit(1);
            topovisit(n, reversed,seen);
        }
        int[] topo = new int[reversed.size()];
        int i = 0;
        for (int e : reversed)
            topo[i++] = e;
        return topo;
    }

    // get rid of extraneous edges with a bfs
    // this is now a dag from root down
    int[][] rooted(ArrayList<Integer>[] graph, int s) {
        BitSet seen = new BitSet(graph.length);
        Queue<Integer> todo = new LinkedList<>();
        todo.add(s);
        seen.set(s);

        //        System.out.println("visit rooted");
        while (! todo.isEmpty()) {
            int cur = todo.remove();
            seen.set(cur);
            Iterator<Integer> cur_adj_it = graph[cur].iterator();
            while (cur_adj_it.hasNext()) {
                int adj = cur_adj_it.next();
                if (! seen.get(adj)) {
                    todo.add(adj);
                } else {
                    cur_adj_it.remove();
                }
            }
        }
        int[][] graph1 = new int[graph.length][];
        for (int i = 1; i <= NODES; i++) {
            graph1[i] = new int[graph[i].size()];
            int j = 0;
            for (int e : graph[i])
                graph1[i][j++] = e;
        }
        return graph1;
    }
    /*
     starting at the root:
     strangeness Leaf =  1  if black
                      | -1 if white
     strangeness (Node e subs)
          =  max ( max (strangeness subs)  // dont pick this node, disconnect
                 , if e == black then
                         all neg subtrees
                   else
                         all pos subtrees
                 )
    */

    // this is a monoid...
    class DPInfo {
        BitSet subtreeNodes;
        int parity;
        boolean picked;
        int lastpicked; // node this info is propagated from
        DPInfo() {
            subtreeNodes = new BitSet(NODES);
            parity = 0;// warning-- not initialized
            picked = false;
            lastpicked = 0;// warning-- not initialized
        }
        DPInfo(DPInfo copy) {
            this.subtreeNodes = copy.subtreeNodes;
            this.parity = copy.parity;
            this.picked = copy.picked;
            this.lastpicked = copy.lastpicked;
        }
    }

    static void printDPInfo(DPInfo info) {
        System.out.println(Math.abs(info.parity));
        System.out.println(info.subtreeNodes.cardinality());

         for (int i = info.subtreeNodes.nextSetBit(1); i >= 0; i = info.subtreeNodes.nextSetBit(i+1)) {
            System.out.print(i + " ");
         }
        System.out.println("");
    }

    static void debugDPInfo(DPInfo info) {
        System.out.println(info.parity);

        for (int i = info.subtreeNodes.nextSetBit(1); i >= 0; i = info.subtreeNodes.nextSetBit(i+1)) {
            System.out.print(i + " ");
        }
        System.out.println("");
    }

    int parity(int i) {
        return isblack(i) ? 1 : -1;
    }
    // bottom-up tree, accumulating strangeness info
    DPInfo maxstrange() {
        DPInfo[] dp = new DPInfo[GRAPH.length];

        //        for (int node : revorder) {
        for (int revorder_i = 0; revorder_i < revorder.length; revorder_i++) {
            int node = revorder[revorder_i];
            // dont pick node
            int bestkid = -1;
            int max_strangeness = Integer.MIN_VALUE;
            // pick this node
            DPInfo pickBlack = new DPInfo();
            DPInfo pickWhite = new DPInfo();

            // SINGLE FUSED LOOP
            for (int kid : GRAPH[node]) {

                // case 1: don't pick this node. find kid w/ max strangeness
                int ss = Math.abs(dp[kid].parity);
                if (max_strangeness < ss) {
                    bestkid = kid;
                    max_strangeness = ss;
                }

                // case 2: pick this node. partition kids into
                // positive / negative paraties, pick the best option
                if (dp[kid].picked) {
                    if (dp[kid].parity < 0) { // white = -1
                        pickWhite.subtreeNodes.or(dp[kid].subtreeNodes);
                        pickWhite.subtreeNodes.set(kid);
                        pickWhite.parity += dp[kid].parity;
                    }

                    else if (dp[kid].parity > 0) { // black = 1
                        pickBlack.subtreeNodes.or(dp[kid].subtreeNodes);
                        pickBlack.subtreeNodes.set(kid);
                        pickBlack.parity += dp[kid].parity;
                    }
                }
                else if (! dp[kid].picked) {
                    // parity of lastpicked + missing picks
                    int pathparity = dp[kid].parity;
                    // Path to the target revorder[dp[kid].lastpicked]
                    BitSet path = new BitSet();
                    // lastpicked = index in revorder of the last picked subtree
                    // want: trail of nodes from kid -> parent of lastpicked
                    // startfrom last picked, look at revorder[i] until we reach
                    // revorder[i] == kid
                    int target = revorder[dp[kid].lastpicked];
                    int ri = dp[kid].lastpicked +1;
                    while (true) {
                        // a possible parent
                        int parent = revorder[ri];
                        for (int child : GRAPH[parent]) {
                            // found parent of target
                            if (child == target) {
                                path.set(parent);
                                pathparity += parity(parent);
                                target = parent;
                                break; // exit for loop
                            }
                        }

                        if (parent == kid)
                            break;
                        else
                            ri++;
                    }

                    if  (pathparity < 0) { // white = -1
                        pickWhite.subtreeNodes.or(dp[kid].subtreeNodes);
                        pickWhite.subtreeNodes.or(path);
                        pickWhite.parity += pathparity;
                    }
                    else if (pathparity > 0) { // black = 1
                        pickBlack.subtreeNodes.or(dp[kid].subtreeNodes);
                        pickBlack.subtreeNodes.or(path);
                        pickBlack.parity += pathparity;
                    }
                }
            }

            DPInfo dontpick = bestkid != -1 ? new DPInfo(dp[bestkid]) : null;
            if (dontpick != null)
                dontpick.picked = false;

            pickBlack.subtreeNodes.set(node);
            pickBlack.parity += parity(node);
            pickBlack.lastpicked = revorder_i;
            pickBlack.picked = true;

            pickWhite.subtreeNodes.set(node);
            pickWhite.parity += parity(node);
            pickWhite.lastpicked = revorder_i;
            pickWhite.picked = true;


            DPInfo pickMe = Math.abs(pickBlack.parity) > Math.abs(pickWhite.parity)
                               ? pickBlack
                            : pickWhite;

            DPInfo thisinfo = dontpick == null
                                ? pickMe
                            : Math.abs(dontpick.parity) > Math.abs(pickMe.parity)
                                ? dontpick
                            : pickMe;

            dp[node] = thisinfo;
        }
        // for (int i = 1; i <= NODES; i++) {
        //     printDPInfo(dp[i]);
        //     System.out.println("====================");
        // }
        return dp[ROOT];
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
        printDPInfo(sol.maxstrange());
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
}
