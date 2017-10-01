import java.io.*;
import java.util.*;

public class BFSShortestReach {
    @SuppressWarnings("unchecked")
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int q = in.nextInt();
        while (q-- > 0) {
            int n = in.nextInt();
            int m = in.nextInt();
            ArrayList<Integer>[] graph = new ArrayList[n+1];
            for (int i = 0; i < graph.length; i++) {
                graph[i] = new ArrayList<>();
            }
            while (m-- > 0) {
                int u = in.nextInt();
                int v = in.nextInt();
                graph[u].add(v);
                graph[v].add(u);
            }
            int s = in.nextInt();
            int[] shorts = bfs(graph,6,s);
            for (int i = 1; i <= n; i++) {
                if (i != s) {
                    System.out.print(shorts[i] + " ");
                }
            }
            System.out.println("");
        }
    }
    static int[] bfs(ArrayList<Integer>[] graph, int weight, int s) {
        int[] out = new int[graph.length];
        for (int i = 0; i < out.length; i++) {
            out[i] = -1;
        }
        Queue<Integer> todo = new LinkedList<>();
        todo.add(s);
        out[s] = 0;
        while (! todo.isEmpty()) {
            int cur = todo.remove();
            for (int adj : graph[cur]) {
                if (out[adj] == -1) {
                    out[adj] = out[cur] + weight;
                    todo.add(adj);
                }
            }
        }
        return out;
    }
}
