
abstract class Shape {
    abstract double area();
    abstract double perimeter();
    abstract void draw();
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

    @Override
    void draw() {
        System.out.println("Drawing a Circle with radius: " + radius);
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

    @Override
    void draw() {
        System.out.println("Drawing a Rectangle with length: " + length + " and breadth: " + breadth);
    }
}

public class ShapeDemo {
    public static void main(String[] args) {
        List<Shape> shapes = new ArrayList<>();
        shapes.add(new Circle(5.0));
        shapes.add(new Rectangle(4.0, 6.0));

        for (Shape shape: shapes) {
            System.out.println("Area: " + shape.area());
            System.out.println("Perimeter: " + shape.perimeter());
            shape.draw();
            System.out.println("-------------------");
        }
    }
}


