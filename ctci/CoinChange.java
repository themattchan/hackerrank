import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;
import java.util.regex.*;

public class CoinChange {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int m = in.nextInt();
        int coins[] = new int[m];
        for(int coins_i=0; coins_i < m; coins_i++){
            coins[coins_i] = in.nextInt();
        }

        System.out.println(solve(coins,n));
    }
    static long solve(int[] coins, int n) {
        if (n < 0) return 1;

        long[] table = new long[n+1];
        table[0] = 1;

        // if you permute these lines then order (for the sets of coins) matters
        for (int coin : coins) {
            for (int i = coin; i < table.length; i++) {
                table[i] += table[i-coin];
            }
        }

        return table[n];
    }
}
