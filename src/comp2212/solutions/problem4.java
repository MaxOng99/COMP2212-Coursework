package comp2212.solutions;

import java.util.ArrayList;

public class problem4 {

    public static void main(String[] args) throws Exception {
        InputReader ir = new InputReader();
        ArrayList<ArrayList<Integer>> inputList = ir.readFile("./src/test/input4.txt");

        ArrayList<Integer> firstList = inputList.get(0);
        ArrayList<Integer> outputList = new ArrayList<Integer>();

        while(firstList.size() >= 3) {
            int num1 = firstList.remove(0);
            int num2 = firstList.remove(0);
            int num3 = firstList.remove(0);

            outputList.add(num3);
            outputList.add(num2*2);
            outputList.add(num1*3-1);
        }

        System.out.println(outputList);
    }

}
