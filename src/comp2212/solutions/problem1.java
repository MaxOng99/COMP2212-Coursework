package comp2212.solutions;

import java.util.ArrayList;

public class problem1 {
    public static void main(String[] args) throws Exception {
	// Reading in the input integer stream from the input text file
        InputReader ir = new InputReader();
        ArrayList<ArrayList<Integer>> inputList = ir.readFile("./src/test/input1.txt");

        ArrayList<Integer> firstList = inputList.get(0);
        ArrayList<Integer> secondList = inputList.get(1);
        ArrayList<Integer> shuffleList = new ArrayList<Integer>();
        while (!firstList.isEmpty()) {
            shuffleList.add(firstList.remove(0));
            shuffleList.add(firstList.remove(0));
            shuffleList.add(secondList.remove(0));
        }
        System.out.println(shuffleList);
    }
}
