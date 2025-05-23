(*
NOT WORKING

Avalonia example from chatGpt. Seems more complex that Eto, but currently
maintained.
*)

#r "nuget: Avalonia"
#r "nuget: Avalonia.Desktop"
#r "nuget: Avalonia.Themes.Fluent"

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Threading

module SimpleShapesApp =
    // Create a custom control to draw shapes
    type ShapesCanvas() as this =
        inherit Control()

        override this.Render(context: DrawingContext) =
            // Define brushes and pens
            let rectBrush = Brushes.LightBlue
            let ellipseBrush = Brushes.LightCoral
            let linePen = Pen(Brushes.Black, 2.0)

            // Draw a rectangle
            context.DrawRectangle(rectBrush, null, Rect(50.0, 50.0, 100.0, 60.0))

            // Draw an ellipse
            context.DrawEllipse(ellipseBrush, null, Point(200.0, 80.0), 50.0, 30.0)

            // Draw a line
            context.DrawLine(linePen, Point(50.0, 150.0), Point(250.0, 150.0))

    // Create the main application window
    type MainWindow() as this =
        inherit Window()
        do
            this.Width <- 300.0
            this.Height <- 200.0
            this.Content <- ShapesCanvas()

    // Initialize the app
    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add(Avalonia.Themes.Fluent.FluentTheme())

        override this.OnFrameworkInitializationCompleted() =
            if this.ApplicationLifetime :? IClassicDesktopStyleApplicationLifetime then
                let desktopLifetime = this.ApplicationLifetime :?> IClassicDesktopStyleApplicationLifetime
                desktopLifetime.MainWindow <- MainWindow()
            base.OnFrameworkInitializationCompleted()

    // Entry point
    [<EntryPoint>]
    let main argv =
        AppBuilder.Configure<App>()
            .UsePlatformDetect()
            .LogToTrace()
            .StartWithClassicDesktopLifetime(argv)
