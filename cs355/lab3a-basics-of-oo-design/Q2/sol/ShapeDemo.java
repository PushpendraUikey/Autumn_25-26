
abstract class Shape {
    abstract double area();
    abstract double perimeter();
}

class Circle extends Shape {
    protected double radius;

    public Circle(double radius) {
        this.radius = radius;
    }

    @Override
    double area() {
        return Math.PI * radius * radius;
    }

    @Override
    double perimeter() {
        return 2 * Math.PI * radius;
    }
}

class Rectangle extends Shape {
    protected double length;
    protected double breadth;

    public Rectangle(double length, double breadth) {
        this.length = length;
        this.breadth = breadth;
    }

    @Override
    double area() {
        return length * breadth;
    }

    @Override
    double perimeter() {
        return 2 * (length + breadth);
    }

    public void lengthSet(double length) {
        this.length = length;
    }
    publicvoid breadthSet(double breadth) {
        this.breadth = breadth;
    }
}

class Square extends Rectangle {
    public Square (double side) {
        super(side, side);
    }
    double area() {
        return super.area();
    }
    double perimeter() {
        return super.perimeter();
    }

    @Override
    public void lengthSet(double length) {
        super.lengthSet(length);    // To keep square property
        super.breadthSet(length);
    }

    @Override 
    public void breadthSet(double breadth) {
        super.breadthSet(breadth);  // To keep square property
        super.lengthSet(breadth);
    }
}

public class ShapeDemo {
    public static void main(String[] args) {
        List<Shape> shapes = new ArrayList<>();
        shapes.add(new Circle(5.0));
        shapes.add(new Rectangle(4.0, 6.0));
        shapes.add(new Square(4.0));

        for (Shape shape: shapes) {
            System.out.println("Area: " + shape.area());
            System.out.println("Perimeter: " + shape.perimeter());
        }

        displayViolation();
    }
    private static void resizeRectangle(Rectangle rect, double newLength, double newBreadth) {
        rect.lengthSet(newLength);
        rect.breadthSet(newBreadth);
        System.out.println("Area: " + rect.area());
        System.out.println("Perimeter: " + rect.perimeter());
    }
    private void displayViolation() {
        Rectangle rect = new Rectangle(4.0, 6.0);
        resizeRectangle(rect, 5.0, 7.0);  // Works fine


        Rectangle sq = new Square(5.0);
        resizeRectangle(sq, 6.0, 7.0);    // Violates LSP
    }
}

/*
 * Definition (informal): If class S is a subclass of T, then anywhere you can use T, you should also be 
 * able to use S without changing the program’s correctness.
 */