/**
 * Used to manage an AVL tree.
 */
public class StockDataManager {
    private AVLTree avlTree;

    /**
     * Initalizes a tree with no elements.
     */
    public StockDataManager() {
        avlTree = new AVLTree();
    }

    /** Add or update a stock
     */
    public void addOrUpdateStock(String symbol, double price, long volume, long marketCap) {
        Stock existingStock = avlTree.search(symbol);
        if (existingStock != null) {
            existingStock.setPrice(price);
            existingStock.setVolume(volume);
            existingStock.setMarketCap(marketCap);
        } else {
            Stock newStock = new Stock(symbol, price, volume, marketCap);
            avlTree.insert(newStock);
        }
    }

    /** Remove a stock
     */
    public void removeStock(String symbol) {
        avlTree.delete(symbol);
    }

    /** Search for a stock
     */
    public Stock searchStock(String symbol) {
        return avlTree.search(symbol);
    }

    /** Update stock details
     */
    public void updateStock(String symbol, String newSymbol, double newPrice, long newVolume, long newMarketCap) {
        Stock stock = avlTree.search(symbol);
        // if the given stock exists
        if (stock != null) {
            // if the new symbol is different
            if(!symbol.equals(newSymbol)) {
                // if the new symbol already exists
                if(avlTree.search(newSymbol) != null) return;
            }
            avlTree.delete(symbol);
            stock.setSymbol(newSymbol);
            stock.setPrice(newPrice);
            stock.setVolume(newVolume);
            stock.setMarketCap(newMarketCap);
            avlTree.insert(stock);
        }
    }

    /** Main method for testing
     */
    public static void main(String[] args) {
        StockDataManager manager = new StockDataManager();
        manager.addOrUpdateStock("AAPL", 150.0, 1000000, 2500000000L);
        manager.addOrUpdateStock("GOOGL", 2800.0, 500000, 1500000000L);
        System.out.println(manager.searchStock("AAPL"));
        System.out.println(manager.searchStock("GOOGL"));
        manager.removeStock("AAPL");
        System.out.println(manager.searchStock("AAPL"));
        System.out.println(manager.searchStock("GOOGL"));
    }
}
