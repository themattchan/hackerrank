import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;
import java.util.regex.*;

public class Solution {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int s = in.nextInt();
        int t = in.nextInt();
        int a = in.nextInt();
        int b = in.nextInt();
        int m = in.nextInt();
        int n = in.nextInt();
        int apples = 0;
        for (int i = 0; i < m; i++){
            int apple = in.nextInt();
            if (a + apple >= s && a + apple <= t) apples++;
        }
        System.out.println(apples);
        int oranges = 0;
        for (int i = 0; i < n; i++){
            int orange = in.nextInt();
            if (a + orange >= s && a + orange <= t) oranges++;
        }
        System.out.println(oranges);
    }
}
