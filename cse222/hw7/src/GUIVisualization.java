import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

/**
 * A Swing GUI for visualizing performance data with line or scatter plots.
 */
public class GUIVisualization extends JFrame {
    private List<PlotData> data = new ArrayList<PlotData>();
    private String plotType; // Type of plot ("line" or "scatter")

    private static class PlotData {
        public static class Point {
            public long x;
            public long y;
            public Point(long _x, long _y) { x = _x; y = _y; }
        }
        public List<Point> points = new ArrayList<Point>();
        public Color color;
        public PlotData(Color col) { color = col; }
        public void add(long x, long y) { points.add(new Point(x, y)); }
    }

    public GUIVisualization(String plotType) {
        this.plotType = plotType; // Set the plot type

        setTitle("Performance Graph Visualization"); // Set the title of the window
        setSize(800, 600); // Set the size of the window
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // Set the default close operation
        setLocationRelativeTo(null); // Center the window on the screen
    }

    /**
     * Adds data for plotting with a specific color.
     * @param col The color of the data points.
     */
    public void addData(Color col) {
        data.add(new PlotData(col));
    }

    /**
     * Adds a data point for a specific data set.
     * @param i The index of the data set.
     * @param x The x-coordinate of the data point.
     * @param y The y-coordinate of the data point.
     */
    public void addDataPoint(int i, long x, long y) {
        data.get(i).add(x, y);
    }

    @Override
    public void paint(Graphics g) {
        super.paint(g); // Call the superclass's paint method
        drawGraph(g); // Draw the graph
    }

    private void drawGraph(Graphics g) {
        int width = getWidth(); // Get the width of the window
        int height = getHeight(); // Get the height of the window
        int padding = 50; // Padding around the graph
        int labelPadding = 20; // Padding for labels

        long maxY = getMaxYValue();

        Graphics2D g2 = (Graphics2D) g; // Cast Graphics to Graphics2D for better rendering
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON); // Enable anti-aliasing

        // Draw white background for the graph
        g2.setColor(Color.WHITE);
        g2.fillRect(padding + labelPadding, padding, width - 2 * padding - labelPadding, height - 2 * padding - labelPadding);
        g2.setColor(Color.BLACK);

        // Create hatch marks and grid lines for y axis.
        int numberYDivisions = 10; // Number of divisions for the y-axis
        for (int i = 0; i < numberYDivisions + 1; i++) {
            int x0 = padding + labelPadding;
            int x1 = width - padding;
            int y0 = height - ((i * (height - padding * 2 - labelPadding)) / numberYDivisions + padding);
            int y1 = y0;
            if (data.get(0).points.size() > 0) {
                g2.setColor(Color.LIGHT_GRAY); // Set color for grid lines
                g2.drawLine(padding + labelPadding + 1 + labelPadding, y0, x1, y1); // Draw grid line
                g2.setColor(Color.BLACK); // Set color for labels
                String yLabel = ((int) ((maxY * ((i * 1.0) / numberYDivisions)) * 100)) / 100.0 + ""; // Generate y-axis label
                FontMetrics metrics = g2.getFontMetrics(); // Get font metrics for label width
                int labelWidth = metrics.stringWidth(yLabel);
                g2.drawString(yLabel, x0 - labelWidth - 5, y0 + (metrics.getHeight() / 2) - 3); // Draw y-axis label
            }
        }

        // Create hatch marks and grid lines for x axis.
        for (int i = 0; i < data.get(0).points.size(); i++) {
            if (data.get(0).points.size() > 1) {
                int x0 = i * (width - padding * 2 - labelPadding) / (data.get(0).points.size() - 1) + padding + labelPadding;
                int x1 = x0;
                int y0 = height - padding - labelPadding;
                int y1 = y0 - 4;
                if ((i % ((int) ((data.get(0).points.size() / 20.0)) + 1)) == 0) {
                    g2.setColor(Color.LIGHT_GRAY); // Set color for grid lines
                    g2.drawLine(x0, height - padding - labelPadding - 1 - labelPadding, x1, padding); // Draw grid line
                    g2.setColor(Color.BLACK); // Set color for labels
                    String xLabel = data.get(0).points.get(i).x + ""; // Generate x-axis label
                    FontMetrics metrics = g2.getFontMetrics(); // Get font metrics for label width
                    int labelWidth = metrics.stringWidth(xLabel);
                    g2.drawString(xLabel, x0 - labelWidth / 2, y0 + metrics.getHeight() + 3); // Draw x-axis label
                }
                g2.drawLine(x0, y0, x1, y1); // Draw x-axis hatch mark
            }
        }

        // Draw axis lines.
        g2.drawLine(padding + labelPadding, height - padding - labelPadding, padding + labelPadding, padding); // y-axis
        g2.drawLine(padding + labelPadding, height - padding - labelPadding, width - padding, height - padding - labelPadding); // x-axis

        // Draw the actual graph.
        Stroke oldStroke = g2.getStroke();
        g2.setStroke(new BasicStroke(2f)); // Set stroke for the graph

        long xOrigin = data.get(0).points.get(0).x;

        for(PlotData d : data) {
            g2.setColor(d.color); // Set color for the graph
            if (plotType.equals("line")) {
                for (int i = 0; i < d.points.size() - 1; i++) {
                    int x1 = (int) ((d.points.get(i).x - xOrigin) * (width - padding * 2 - labelPadding) / (d.points.get(d.points.size() - 1).x - xOrigin)) + padding + labelPadding;
                    int y1 = height - padding - labelPadding - (int) ((d.points.get(i).y * 1.0) / maxY * (height - padding * 2 - labelPadding));
                    int x2 = (int) ((d.points.get(i + 1).x - xOrigin) * (width - padding * 2 - labelPadding) / (d.points.get(d.points.size() - 1).x - xOrigin)) + padding + labelPadding;
                    int y2 = height - padding - labelPadding - (int) ((d.points.get(i + 1).y * 1.0) / getMaxYValue() * (height - padding * 2 - labelPadding));
                    g2.drawLine(x1, y1, x2, y2); // Draw line between data points
                }
            } else if (plotType.equals("scatter") && d.points.size() > 0) {
                for (int i = 0; i < d.points.size(); i++) {
                    int x = (int) ((d.points.get(i).x - xOrigin) * (width - padding * 2 - labelPadding) / (d.points.get(d.points.size() - 1).x - xOrigin)) + padding + labelPadding;
                    int y = height - padding - labelPadding - (int) ((d.points.get(i).y * 1.0) / maxY * (height - padding * 2 - labelPadding));
                    g2.fillOval(x - 3, y - 3, 6, 6); // Draw data point as a small circle
                }
            }
        }

        g2.setStroke(oldStroke); // Restore original stroke
    }

    private long getMaxYValue() {
        long max = Long.MIN_VALUE; // Initialize max value to minimum possible value
        for (PlotData d : data) {
            for (PlotData.Point p : d.points) {
                max = Math.max(max, p.y); // Find maximum y value
            }
        }
        return max; // Return maximum y value
    }

    /**
     * Analyzes the performances of all operations from given input file
     * and plots them on a graph each color representing different operation.
     * (Red for add, blue for search, green for remove, yellow for update.)
     */
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            String plotType = "scatter"; // Change to "scatter" for scatter plot
            GUIVisualization frame = new GUIVisualization(plotType); // Create a new instance of GUIVisualization
            CommandParser cm = new CommandParser(args[0]);

            frame.addData(Color.RED);
            frame.addData(Color.BLUE);
            frame.addData(Color.GREEN);
            frame.addData(Color.YELLOW);

            for (int i = 1; i <= 100; i++) {
                long x = i * 10;
                PerformanceAnalyzer.Report report = PerformanceAnalyzer.analyze(cm, (int)x);
                frame.addDataPoint(0, x, report.addTime);
                frame.addDataPoint(1, x, report.searchTime);
                frame.addDataPoint(2, x, report.removeTime);
                frame.addDataPoint(3, x, report.updateTime);
            }

            frame.setVisible(true);
        });
    }
}
