package comp2212.solutions;

import java.io.*;
import java.lang.reflect.Array;
import java.util.ArrayList;

public class InputReader {

    public ArrayList<ArrayList<Integer>> readFile(String filePath) throws Exception {
        ArrayList<ArrayList<Integer>> inputList = new ArrayList<ArrayList<Integer>>();

        File input = new File(filePath);

        BufferedReader br = new BufferedReader(new FileReader(input));

        String st;
        while ((st = br.readLine()) != null) {
            ArrayList<Integer> colList = new ArrayList<Integer>();
            String[] splitNum = st.split(" ");
            for (String num : splitNum) {
                colList.add(Integer.parseInt(num));
            }
            inputList.add(colList);
        }
        return inputList;
    }

}
