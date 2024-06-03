/**
 * Analyzes the performance of the stock data management system.
 */
public class PerformanceAnalyzer {
    /**
     * A class for holding time values for each operation.
     */
    public static class Report {
        public long addTime = 0;
        public long searchTime = 0;
        public long removeTime = 0;
        public long updateTime = 0;
    }

    /**
     * Analyzes the performance of the tree operations with the given CommandParser and starting size.
     * @param cm The CommandParser to run the commands from a file.
     * @param startSize The initial size of the tree.
     * @return A PerformanceAnalyzer.Report object containing performance metrics.
     */
    public static Report analyze(CommandParser cm, int startSize) {
        long startTime, endTime;
        StockDataManager manager = new StockDataManager();
        Report report = new Report();
        int nAdd = 0;
        int nRemove = 0;
        int nSearch = 0;
        int nUpdate = 0;

        // get the tree to the starting size
        for(int i = 0; i < startSize; ++i) {
            manager.addOrUpdateStock("SYMA" + i, Math.random() * 100, (long) (Math.random() * 1000000), (long) (Math.random() * 1000000000));
        }

        // start from line zero of the command file
        cm.restart();

        while(cm.hasNext()) {
            startTime = System.nanoTime();
            int no = cm.parseNext(manager); // returns the id of the operation performed
            endTime = System.nanoTime();
            long time = endTime - startTime;
            switch(no) {
                case 0:
                    report.addTime += time;
                    nAdd++;
                    break;
                case 1:
                    report.removeTime += time;
                    nRemove++;
                    break;
                case 2:
                    report.searchTime += time;
                    nSearch++;
                    break;
                case 3:
                    report.updateTime += time;
                    nUpdate++;
                    break;
            }
        }

        // get the average time by dividing
        report.addTime /= nAdd > 0 ? nAdd : 1;
        report.removeTime /= nRemove > 0 ? nRemove : 1;
        report.searchTime /= nSearch > 0 ? nSearch : 1;
        report.updateTime /= nUpdate > 0 ? nUpdate : 1;

        return report;
    }
}
