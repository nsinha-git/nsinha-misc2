package amazon;

/**
 * Created by nsinha on 3/17/17.
 */

// IMPORT LIBRARY PACKAGES NEEDED BY YOUR PROGRAM
// SOME CLASSES WITHIN A PACKAGE MAY BE RESTRICTED
// DEFINE ANY CLASS AND METHOD NEEDED
// CLASS BEGINS, THIS CLASS IS REQUIRED
import java.util.LinkedList;
import java.util.List;

class Node {
    Node (int v) {
        val = v;
    }
    int val ;

    Node left = null;
    Node right = null;
}
public class Solution
{


    Node root = null;

    void insertIntoTree(Node tree, Node node) {
        if (tree == null) {
            root = node;
            return;
        } else if(tree.val == node.val){


        } else if (tree.val > node.val){
            if(tree.left != null){
                insertIntoTree(tree.left, node);
            } else {
                tree.left = node;
            }

        } else if (tree.val < node.val) {
            if (tree.right != null) {
                insertIntoTree(tree.right, node);
            } else {
                tree.right = node;
            }
        }
        return;
    }

    Solution(int[] values, int n)  {
        for (int i =0; i< n; i++) {
            int v = values[i];
            Node node = new Node(v);
            insertIntoTree(root,node);
        }
    }



    List<Integer> findPath(Node tree, int n1, List<Integer> prefix){
        if (tree == null) return null;

        prefix.add(tree.val);
        if (tree.val == n1) {
            return  prefix;
        }


        if (tree.val < n1) return findPath(tree.right,n1, prefix);
        if (tree.val > n1) return findPath(tree.left,n1, prefix);
        return null;
    }


    public static int bstDistance(int[] values, int n, int node1, int node2)
    {
        // WRITE YOUR CODE HERE
        Solution t =new Solution(values, n);
        List<Integer> l1 = new LinkedList<>();
        List<Integer> l2 = new LinkedList<>();

        List<Integer> p1 = t.findPath(t.root, node1, l1);
        List<Integer> p2 = t.findPath(t.root, node2, l2);
        if(p1 == null || p2 == null) return -1;
        boolean cond = true;
        while (cond) {
            if(p1.get(0) == p2.get(0)) {
                p1.remove(0);
                p2.remove(0);
            } else {
                cond = false;
            }
        }
        return p1.size() + p2.size();
    }

    public static void main(String[] args) {

        int[] test = {5,6,3,1,2,4};

        int x = bstDistance(test, test.length, 2,4);
        System.out.println(x);

    }

}
