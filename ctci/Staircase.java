import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;
import java.util.regex.*;

public class Staircase {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int s = in.nextInt();
        for(int a0 = 0; a0 < s; a0++){
            int n = in.nextInt();
            System.out.println(solve(n));
        }
    }
    static int solve(int n) {
        int[] memo = new int[n+1];
        memo[0] = 1;
        for (int i = 1; i <= n; i++) {
            if (i - 1 >= 0)
                memo[i] += memo[i-1];
            if (i - 2 >= 0)
                memo[i] += memo[i-2];
            if (i - 3 >= 0)
                memo[i] += memo[i-3];
        }
        return memo[n];
    }
}
