(*
Work in progress. Easier than avalonia, but still work. Drawing on the console
for now...
*)

#r "nuget: Eto.Forms"
#r "nuget: Eto.Platform.Gtk"

open Eto.Forms
open Eto.Drawing

type MainForm() as this =
    inherit Form()

    do
        this.Title <- "Simple Drawing Example"
        this.ClientSize <- Size(800, 600)

        let drawable = new Drawable()
        drawable.Paint.Add(fun args ->
            let graphics = args.Graphics

            let redBrush = Brushes.Red
            graphics.FillEllipse(redBrush, RectangleF(100f, 100f, 200f, 200f))

            let blueBrush = Brushes.Blue
            graphics.FillRectangle(blueBrush, RectangleF(350f, 100f, 200f, 100f))

            let greenPen = Pens.Green
            graphics.DrawLine(greenPen, PointF(50f, 400f), PointF(750f, 400f))
        )

        this.Content <- drawable

type Colour = Red | Green | Blue
type Square = {
    Size: int
    Colour: Colour
}
type Circle = {
    Center: int * int
    Colour: Colour
}
type Shape = Square | Circle
type Drawing =
    {
        Shapes: list<Shape>
    }


// let app = new Application()
// let mainForm = new MainForm()
// app.Run(mainForm)
