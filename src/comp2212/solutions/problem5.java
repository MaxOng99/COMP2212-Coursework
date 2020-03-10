package comp2212.solutions;

import java.util.ArrayList;

public class problem5 {

    public static void main(String[] args) throws Exception {
        InputReader ir = new InputReader();
        ArrayList<ArrayList<Integer>> inputList = ir.readFile("./src/test/input5.txt");


        ArrayList<Integer> firstList = inputList.get(0);
        ArrayList<Integer> outputList = new ArrayList<Integer>();

        int sum = 0;

        while (!firstList.isEmpty()) {
            sum += firstList.remove(0);
            outputList.add(sum);
        }

        System.out.println(outputList);
    }
}
