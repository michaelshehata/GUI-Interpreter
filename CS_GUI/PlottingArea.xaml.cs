using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Series;
using OxyPlot.Wpf;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for PlottingArea.xaml
    /// </summary>
    public partial class PlottingArea : UserControl
    {
        private PlotModel _model;
        private readonly OxyColor _functionColour = OxyColors.SteelBlue;
        private const double _lineThickness = 2;
        public bool LockAspectRatio { get; set; } = false;
        private bool _axesInitialised = false;


        /// <summary>
        /// Specifies the algorithm used to interpolate values between data points.
        /// </summary>
        /// <remarks>Use <see cref="InterpolationMode"/> to control how intermediate values are calculated
        /// when generating or transforming data. The available modes determine the smoothness and characteristics of
        /// the interpolation.</remarks>
        public enum InterpolationMode
        {
            Linear,
            Spline
        }

        /// <summary>
        /// Gets or sets the interpolation mode used for rendering operations.
        /// </summary>
        public InterpolationMode Interpolation { get; set; } = InterpolationMode.Linear;

        /// <summary>
        /// Initializes a new instance of the <see cref="PlottingArea"/> class.
        /// </summary>
        /// <remarks>Sets up the plotting area with a default plot model and associates it with the plot
        /// view control.</remarks>
        public PlottingArea()
        {
            InitializeComponent();
            _model = new PlotModel { Title = "Function Plot" };
            PlotViewControl.Model = _model;
        }


        /// <summary>
        /// Takes in a collection of (x, y) points representing the results of a function evaluation to be plotted, and the x-axis range.
        /// Handles plotting these points within the specified range, managing discontinuities, and adjusting axis scaling.
        /// </summary>
        /// <param name="points"></param>
        /// <param name="xMin"></param>
        /// <param name="xMax"></param>
        public void PlotFunction(IEnumerable<Tuple<double, double>> points,
                                 double xMin, double xMax)
        {


            // early exit if plot not ready
            if (!PlotViewControl.IsLoaded ||
                PlotViewControl.ActualWidth <= 1 ||
                PlotViewControl.ActualHeight <= 1)
            {
                return;
            }


            _model.Series.Clear();

            const double jumpThreshold = 10.0;   // asymptote detection
            const double yVisualLimit = 10.0;   // ignore extreme values for scaling

            bool hasDiscontinuities = false; // for spline safety check

            var validYsForScaling = new List<double>();

            LineSeries currentSeries = CreateSeries();
            _model.Series.Add(currentSeries);

            DataPoint? lastPoint = null;

            // process points
            foreach (var p in points)
            {
                double x = p.Item1;
                double y = p.Item2;

                // Invalid values -> break line
                if (double.IsNaN(y) || double.IsInfinity(y))
                {
                    hasDiscontinuities = true;
                    currentSeries = CreateSeries();
                    _model.Series.Add(currentSeries);
                    lastPoint = null;
                    continue;
                }

                // Large jump -> likely asymptote
                if (lastPoint.HasValue &&
                    Math.Abs(y - lastPoint.Value.Y) > jumpThreshold)
                {
                    hasDiscontinuities = true;
                    currentSeries = CreateSeries();
                    _model.Series.Add(currentSeries);
                }

                currentSeries.Points.Add(new DataPoint(x, y));
                lastPoint = new DataPoint(x, y);

                // Collect reasonable values for Y scaling
                if (Math.Abs(y) <= yVisualLimit)
                    validYsForScaling.Add(y);
            }

            // Axis scaling (INITIAL ONLY)
            if (!_axesInitialised)
            {
                _model.Axes.Clear();

                var xAxis = new LinearAxis
                {
                    Position = AxisPosition.Bottom,
                    Minimum = xMin,
                    Maximum = xMax,
                    MajorGridlineStyle = LineStyle.Solid,
                    MinorGridlineStyle = LineStyle.Dot
                };

                double yMin = -1;
                double yMax = 1;

                if (validYsForScaling.Any())
                {
                    yMin = validYsForScaling.Min();
                    yMax = validYsForScaling.Max();

                    if (Math.Abs(yMax - yMin) < 1e-6)
                    {
                        yMin -= 1;
                        yMax += 1;
                    }
                    else
                    {
                        double padding = (yMax - yMin) * 0.1;
                        yMin -= padding;
                        yMax += padding;
                    }
                }

                double yMinFinal = yMin;
                double yMaxFinal = yMax;

                // 1:1 aspect ratio lock
                if (LockAspectRatio)
                {
                    double xRange = xMax - xMin;
                    double yRange = yMax - yMin;
                    if (yRange <= 0) yRange = 1;

                    double plotWidth = PlotViewControl.ActualWidth;
                    double plotHeight = PlotViewControl.ActualHeight;

                    if (plotWidth <= 0 || plotHeight <= 0)
                    {
                        plotWidth = 800;
                        plotHeight = 600;
                    }

                    double plotAspect = plotWidth / plotHeight;
                    double dataAspect = xRange / yRange;

                    if (dataAspect > plotAspect)
                    {
                        double newYRange = xRange / plotAspect;
                        double yMid = (yMin + yMax) / 2.0;

                        yMinFinal = yMid - newYRange / 2.0;
                        yMaxFinal = yMid + newYRange / 2.0;
                    }
                }

                LinearAxis yAxis;

                if (LockAspectRatio)
                {
                    yAxis = new LinearAxis
                    {
                        Position = AxisPosition.Left,
                        Minimum = yMinFinal,
                        Maximum = yMaxFinal,
                        MajorGridlineStyle = LineStyle.Solid,
                        MinorGridlineStyle = LineStyle.Dot
                    };
                }
                else
                {
                    // let OxyPlot handle y scaling 
                    yAxis = new LinearAxis
                    {
                        Position = AxisPosition.Left,
                        MajorGridlineStyle = LineStyle.Solid,
                        MinorGridlineStyle = LineStyle.Dot
                    };
                }


                _model.Axes.Add(xAxis);
                _model.Axes.Add(yAxis);

                _axesInitialised = true;
            }



            // disable spline if unsafe
            if (hasDiscontinuities && Interpolation == InterpolationMode.Spline)
            {
                foreach (var s in _model.Series.OfType<LineSeries>())
                    s.InterpolationAlgorithm = null;
            }

            _model.InvalidatePlot(true);
        }

        /// <summary>
        /// Resets the axis initialisation flag, forcing axes to be recalculated on the next plot.
        /// </summary>
        public void ResetAxes()
        {
            _axesInitialised = false;
        }

        /// <summary>
        /// LineSeries factory method
        /// </summary>
        /// <returns></returns>
        private LineSeries CreateSeries()
        {
            var series = new LineSeries
            {
                Color = _functionColour,
                StrokeThickness = _lineThickness,
                MarkerType = MarkerType.None
            };

            if (Interpolation == InterpolationMode.Spline)
            {
                series.InterpolationAlgorithm = InterpolationAlgorithms.CanonicalSpline;
            }

            return series;
        }

        /// <summary>
        /// Given a point (x0, f(x0)) and the slope of the tangent line at that point, plots the tangent line over the specified x-range.
        /// </summary>
        /// <param name="x0"></param>
        /// <param name="fx0"></param>
        /// <param name="slope"></param>
        /// <param name="xMin"></param>
        /// <param name="xMax"></param>
        public void PlotTangentLine(
            double x0,
            double fx0,
            double slope,
            double xMin,
            double xMax)
        {
            var tangentSeries = new LineSeries
            {
                Color = OxyColors.IndianRed,
                StrokeThickness = 2,
                LineStyle = LineStyle.Dash,
                Title = "Tangent",
                Tag = "Tangent"
            };

            double y1 = fx0 + slope * (xMin - x0);
            double y2 = fx0 + slope * (xMax - x0);

            tangentSeries.Points.Add(new DataPoint(xMin, y1));
            tangentSeries.Points.Add(new DataPoint(xMax, y2));

            var pointSeries = new ScatterSeries
            {
                MarkerType = MarkerType.Circle,
                MarkerSize = 5,
                MarkerFill = OxyColors.IndianRed,
                Tag = "Tangent"
            };

            pointSeries.Points.Add(new ScatterPoint(x0, fx0));

            _model.Series.Add(tangentSeries);
            _model.Series.Add(pointSeries);

            _model.InvalidatePlot(true);
        }

        /// <summary>
        /// Clear all tangent lines from the plot.
        /// </summary>
        public void ClearTangents()
        {
            var tangents = _model.Series
                .Where(s => s.Tag?.ToString() == "Tangent")
                .ToList();

            foreach (var series in tangents)
            {
                _model.Series.Remove(series);
            }

            _model.InvalidatePlot(true);
        }

        /// <summary>
        /// Given a collection of (x, y) points representing a function, plots the area under the curve between limits a and b. Displays the approximate area value in a mouse-over label.
        /// </summary>
        /// <param name="points"></param>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="areaValue"></param>
        /// <param name="expression"></param>
        public void PlotIntegrationArea(
            IEnumerable<Tuple<double, double>> points,
            double a,
            double b,
            double areaValue,
            string expression)
        {
            var areaSeries = new AreaSeries
            {
                Color = OxyColor.FromAColor(80, OxyColors.ForestGreen),
                StrokeThickness = 1,
                LineStyle = LineStyle.Solid,
                Tag = "Integral",

                // Mouse-over label
                TrackerFormatString =
                    $"∫[{a}, {b}] {expression} dx\n≈ {areaValue:F6}"
            };

            foreach (var p in points)
            {
                double x = p.Item1;
                double y = p.Item2;

                if (x < a || x > b)
                    continue;

                if (double.IsNaN(y) || double.IsInfinity(y))
                    continue;

                areaSeries.Points.Add(new DataPoint(x, y));
                areaSeries.Points2.Add(new DataPoint(x, 0));
            }

            _model.Series.Add(areaSeries);
            _model.InvalidatePlot(true);
        }


        /// <summary>
        /// Clear all integration areas from the plot.
        /// </summary>
        public void ClearIntegrals()
        {
            var integrals = _model.Series
                .Where(s => s.Tag?.ToString() == "Integral")
                .ToList();

            foreach (var s in integrals)
            {
                _model.Series.Remove(s);
            }

            _model.InvalidatePlot(true);
        }




    }
}