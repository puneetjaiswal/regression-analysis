import com.google.gson.Gson;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class APIReader {

  static final String apiBase =
      "https://query1.finance.yahoo.com/v10/finance/quoteSummary/%s?region=US&modules=defaultKeyStatistics,assetProfile,financialData";

  static String getURLContent(String ticker) throws Exception {
    String url = String.format(apiBase, ticker);
    return URLConnectionReader.getText(url);
  }

  public static void main(String[] args) throws Exception {
    Scanner scn = new Scanner(APIReader.class.getResourceAsStream("ticker.txt"));
    Gson gson = new Gson();
    // TTM = trailing twelve months
    System.out.println(
        "Symbol\tCompany\tNum_Employees\tRevenue_Million_TTM\tprofit_Million_TTM\texpanse_million\tenterprise_val_Million");
    while (scn.hasNext()) {
      String[] tickInfo = scn.nextLine().split("\t");
      String profileData = getURLContent(tickInfo[0]);
      Map<String, Object> profileDataMap = new HashMap<>();
      try {
        profileDataMap = gson.fromJson(profileData, profileDataMap.getClass());
        Double numEmp =
            (Double) ((Map) ((Map) ((List) ((Map) profileDataMap.get("quoteSummary")).get("result"))
                .get(0)).get("assetProfile")).get("fullTimeEmployees");
        Double revenue =
            (Double) ((Map) ((Map) ((Map) ((List) ((Map) profileDataMap.get("quoteSummary"))
                .get("result")).get(0)).get("financialData")).get("totalRevenue")).get("raw");
        Double profit =
            (Double) ((Map) ((Map) ((Map) ((List) ((Map) profileDataMap.get("quoteSummary"))
                .get("result")).get(0)).get("financialData")).get("grossProfits")).get("raw");
        Double enterpriseVal =
            (Double) ((Map) ((Map) ((Map) ((List) ((Map) profileDataMap.get("quoteSummary"))
                .get("result")).get(0)).get("defaultKeyStatistics")).get("enterpriseValue"))
                .get("raw");
        int million = 1_000_000;
        System.out.println(
            tickInfo[0] + "\t" + tickInfo[1] + "\t" + numEmp.intValue() + "\t" + (int) (revenue
                / million) + "\t" + profit / million + "\t" + (int)((revenue - profit)/million) + "\t"
                + enterpriseVal / million);
        //        System.out.println(revenue);
      } catch (Exception e) {
        //        System.err.println(symbol);
        //        e.printStackTrace();
      }

    }
  }

  public static class URLConnectionReader {
    public static String getText(String url) throws Exception {
      URL website = new URL(url);
      URLConnection connection = website.openConnection();
      StringBuilder response = new StringBuilder();
      try (
          BufferedReader in = new BufferedReader(
              new InputStreamReader(connection.getInputStream()))) {
        String inputLine;

        while ((inputLine = in.readLine()) != null) {
          response.append(inputLine);
        }
      }
      return response.toString();
    }
  }
}
