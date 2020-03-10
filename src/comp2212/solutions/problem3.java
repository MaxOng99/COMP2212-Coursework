package comp2212.solutions;

import java.util.ArrayList;

public class problem3 {

    public static void main(String[] args) throws Exception {
        InputReader ir = new InputReader();
        ArrayList<ArrayList<Integer>> inputList = ir.readFile("./src/test/input3.txt");

        ArrayList<Integer> firstList = inputList.get(0);
        ArrayList<Integer> secondList = inputList.get(1);
        ArrayList<Integer> shuffleList = new ArrayList<Integer>();

        if (secondList.size() >= 2) {
            shuffleList.add(0);
            shuffleList.add(secondList.remove(1));
            shuffleList.add(firstList.remove(0));

            while (secondList.size() >= 3) {
                secondList.remove(1);
                shuffleList.add(secondList.remove(1));
                shuffleList.add(firstList.remove(0));
            }
        }

        System.out.println(shuffleList);
    }
}
