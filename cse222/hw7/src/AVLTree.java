/**
 * Implements an AVL Tree data structure to store Stock objects.
 */
public class AVLTree {
    private class Node {
        Stock stock;
        Node left, right;
        int height;

        Node(Stock stock) {
            this.stock = stock;
            this.height = 1;
        }
    }

    private Node root;

    /**
     * Inserts a Stock object into the AVL Tree.
     * @param stock The Stock object to be inserted.
     */
    public void insert(Stock stock) {
        root = insert(root, stock);
    }

    private Node insert(Node node, Stock stock) {
        // Implementation of AVL Tree insertion logic
        if(node == null) return new Node(stock);

        // bst insertion
        int cmp = stock.getSymbol().compareTo(node.stock.getSymbol());

        if(cmp < 0)
            node.left = insert(node.left, stock);
        else if(cmp > 0)
            node.right = insert(node.right, stock);
        else
            return node;

        updateHeight(node);

        //check if the node became unbalanced
        int balance = getBalance(node);

        if(balance > 1) {
            cmp = stock.getSymbol().compareTo(node.left.stock.getSymbol());
            //left left case
            if(cmp < 0) {
                return rightRotate(node);
            }
            // left right case
            else if(cmp > 0) {
                node.left = leftRotate(node.left);
                return rightRotate(node);
            }
        }

        if(balance < -1) {
            cmp = stock.getSymbol().compareTo(node.right.stock.getSymbol());
            // right right case
            if(cmp > 0) {
                return leftRotate(node);
            }
            // right left case
            else if(cmp < 0) {
                node.right = rightRotate(node.right);
                return leftRotate(node);
            }
        }

        return node;
    }

    /**
     * Deletes a Stock object from the AVL Tree based on its symbol.
     * @param symbol The symbol of the Stock object to be deleted.
     */
    public void delete(String symbol) {
        root = delete(root, symbol);
    }

    // returns the node with the lowest order in its children including itself
    private Node minVal(Node node) {
        Node cur = node;
        while(cur.left != null)
            cur = cur.left;
        return cur;
    }

    private Node delete(Node node, String symbol) {
        // Implementation of AVL Tree deletion logic
        // bst deletion
        if(node == null) return null;

        int cmp = symbol.compareTo(node.stock.getSymbol());
        
        if(cmp < 0)
            node.left = delete(node.left, symbol);
        else if(cmp > 0)
            node.right = delete(node.right, symbol);
        // if the symbol is the same
        // means this node is to be deleted
        else {
            // if node has no children
            if(node.left == null && node.right == null)
                return null;
            // if it has only right child
            else if(node.left == null)
                node = node.right;
            // if it has only left child
            else if(node.right == null)
                node = node.left;
            // if it has two children
            else {
                Node next = minVal(node.right);
                node.stock = next.stock;
                node.right = delete(node.right, next.stock.getSymbol());
            }
        }

        updateHeight(node);

        // check for balance
        int balance = getBalance(node);

        if(balance > 1) {
            // if true then left right case else left left case
            if(getBalance(node.left) < 0)
                node.left = leftRotate(node.left);
            return rightRotate(node);
        }
        if(balance < -1) {
            // if true then right left case else right right case
            if(getBalance(node.right) > 0)
                node.right = rightRotate(node.right);
            return leftRotate(node);
        }
        return node;
    }

    /**
     * Searches for a Stock object in the AVL Tree based on its symbol.
     * @param symbol The symbol of the Stock object to be searched.
     * @return The Stock object if found, otherwise null.
     */
    public Stock search(String symbol) {
        Node result = search(root, symbol);
        return (result != null) ? result.stock : null;
    }

    private Node search(Node node, String symbol) {
        // Implementation of AVL Tree search logic
        if(node == null) return null;
        int cmp = symbol.compareTo(node.stock.getSymbol());
        if(cmp < 0)
            return search(node.left, symbol);
        else if(cmp > 0)
            return search(node.right, symbol);
        return node;
    }

    // Balancing methods (left rotation, right rotation, etc.)
    // Height update and balance factor calculations
    
    private int height(Node node) {
        return node != null ? node.height : 0;
    }

    private void updateHeight(Node node) {
        node.height = Math.max(height(node.left), height(node.right)) + 1;
    }
    
    private int getBalance(Node node) {
        if(node == null)
            return 0;
        return height(node.left) - height(node.right);
    }

    private Node leftRotate(Node node) {
        Node n = node.right;
        node.right = n.left;
        n.left = node;
        updateHeight(node);
        updateHeight(n);
        return n;
    }

    private Node rightRotate(Node node) {
        Node n = node.left;
        node.left = n.right;
        n.right = node;
        updateHeight(node);
        updateHeight(n);
        return n;
    }

    // In-order, pre-order, post-order traversals
    // For example:

    /**
     * Performs a pre-order traversal of the AVL Tree and prints the Stock objects.
     */
    public void preOrderTraversal() {
        preOrderTraversal(root);
    }

    private void preOrderTraversal(Node node) {
        if (node != null) {
            System.out.println(node.stock);
            preOrderTraversal(node.left);
            preOrderTraversal(node.right);
        }
    }

    /**
     * Performs an in-order traversal of the AVL Tree and prints the Stock objects.
     */
    public void inOrderTraversal() {
        inOrderTraversal(root);
    }

    private void inOrderTraversal(Node node) {
        if (node != null) {
            inOrderTraversal(node.left);
            System.out.println(node.stock);
            inOrderTraversal(node.right);
        }
    }

    /**
     * Performs a post-order traversal of the AVL Tree and prints the Stock objects.
     */
    public void postOrderTraversal() {
        postOrderTraversal(root);
    }

    private void postOrderTraversal(Node node) {
        if (node != null) {
            postOrderTraversal(node.left);
            postOrderTraversal(node.right);
            System.out.println(node.stock);
        }
    }
}
