package comp2212.solutions;

import java.util.ArrayList;

public class problem2 {

    public static void main(String[] args) throws Exception {
        InputReader ir = new InputReader();
        ArrayList<ArrayList<Integer>> inputList = ir.readFile("./src/test/input2.txt");

        ArrayList<Integer> firstList = inputList.get(0);
        ArrayList<Integer> secondList = inputList.get(1);
        ArrayList<Integer> thirdList = inputList.get(2);
        ArrayList<Integer> shuffleList = new ArrayList<Integer>();

        for (int i = 0; i < firstList.size(); i++) {
            Integer a = firstList.get(i);
            Integer b = secondList.get(i);
            Integer c = thirdList.get(i);
            shuffleList.add(c);
            shuffleList.add(b);
            shuffleList.add(a);
            shuffleList.add(a+b);
            shuffleList.add(b+c);
        }
        System.out.println(shuffleList);
    }

}
