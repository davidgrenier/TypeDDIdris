module Shape

export
data Shape
    = Rect Double Double
    | Tri Double Double
    | Circ Double
    | Merged Shape Shape

export
rectangle : Double -> Double -> Shape
rectangle = Rect

export
triangle : Double -> Double -> Shape
triangle = Tri

export
circle : Double -> Shape
circle = Circ

export
compose : Shape -> Shape -> Shape
compose = Merged

public export
data ShapeView : Shape -> Type where
    Rectangle : {length, width : _} -> ShapeView (rectangle length width)
    Triangle : {length, width : _} -> ShapeView (triangle length width)
    Circle : {radius : _} -> ShapeView (circle radius)
    Composed : {shape1, shape2 : _} -> (rec1 : ShapeView shape1) ->
                (rec2 : ShapeView shape2) -> ShapeView(compose shape1 shape2)

export
shapeView : (shape : Shape) -> ShapeView shape
shapeView (Rect x y) = Rectangle
shapeView (Tri x y) = Triangle
shapeView (Circ x) = Circle
shapeView (Merged x y) = Composed (shapeView x) (shapeView y)
