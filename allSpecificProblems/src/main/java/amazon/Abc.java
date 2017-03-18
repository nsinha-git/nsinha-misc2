package amazon;

import java.util.Stack;

/**
 * Created by nsinha on 3/17/17.
 */
public class Abc {



    private static Integer tryParseInt(String s) {
        try {
            Integer x = Integer.parseInt(s);
            return x;
        } catch (NumberFormatException e){
           return null;
        }
    }

    private static Character tryParseChar(String s) {
        if(s.length() > 1) return  null;
        if(s.length() == 0) return  null;
        return s.toLowerCase().toCharArray()[0];
    }

    private static void insertStack(Stack<Integer> stk, Integer x) {
        stk.push(x);
    }

    private static boolean adjustStack(Stack<Integer> stk, Character x) {
        if(x == '+') {
            if(stk.size() < 2) return false;
            Integer pop1 = stk.pop();
            Integer peek2 = stk.peek();
            Integer newValue = pop1 + peek2;
            stk.push(pop1);
            stk.push(newValue);
            return true;
        }

        if(x == 'x') {
            if(stk.size() < 1) return false;
            Integer peek1 = stk.peek();
            Integer newValue = 2*  peek1;
            stk.push(newValue);
            return true;
        }
        if(x == 'z') {
            if(stk.size() == 0) return false;
            stk.pop();
            return true;
        }
        return false;
    }


  // METHOD SIGNATURE BEGINS, THIS METHOD IS REQUIRED
    public static int totalScore(String[] blocks, int n)
    {
        Stack<Integer> stack = new Stack<Integer>();
        boolean errorHappened = false;
        for (int i=0; (i < n) && !errorHappened ; i++) {
            String c = blocks[i];
            Integer xInt = tryParseInt(c);
            if (xInt != null) {
                insertStack(stack, xInt);
            } else {
                Character xChar = tryParseChar(c);
                if (xChar != null) {
                    boolean ret = adjustStack(stack, xChar);
                    if (ret == false) errorHappened = true;
                } else {
                    errorHappened = true;
                }
            }
        }

        if(errorHappened == true) throw new RuntimeException("");

        int score = sumTheStack(stack);


        return score;
    }


    private static int sumTheStack(Stack<Integer> stk) {
        int score = 0;
        while (!stk.empty()) {
            score = score + stk.pop();
        }
        return score;
    }

    public static void main(String[] args) {
        String[] test = {"1","2","x"};
        try {
            int score = Abc.totalScore(test, test.length);
            System.out.println(score);
        } catch (Exception e) {
            System.out.println("SOme exception happened");
        }

    }
}
