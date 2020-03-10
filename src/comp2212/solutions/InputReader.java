package comp2212.solutions;

import java.io.*;
import java.lang.reflect.Array;
import java.util.ArrayList;

public class InputReader {

    public ArrayList<ArrayList<Integer>> readFile(String filePath) throws Exception {

        ArrayList<ArrayList<Integer>> inputList = new ArrayList<ArrayList<Integer>>();
        File input = new File(filePath);
        RandomAccessFile reader = new RandomAccessFile(input, "rw");

        String st = reader.readLine();
        for (int i = 0; i < st.replaceAll(" ", "").length(); i++) {
            inputList.add(new ArrayList<Integer>());
        }

        reader.seek(0);

        while ((st = reader.readLine()) != null) {
            String[] splitNum = st.split(" ");
            for (int j = 0; j < splitNum.length; j++) {
                inputList.get(j).add(Integer.parseInt(splitNum[j]));
            }
        }
        return inputList;
    }
}
