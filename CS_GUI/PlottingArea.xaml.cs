using System;
using System.Collections.Generic;
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
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Series;
using OxyPlot.Wpf;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for PlottingArea.xaml
    /// </summary>
    public partial class PlottingArea : UserControl
    {
        private PlotModel _model;

        public enum InterpolationMode
        {
            Linear,
            Spline
        }

        public InterpolationMode Interpolation { get; set; } = InterpolationMode.Linear;
        public PlottingArea()
        {
            InitializeComponent();
            _model = new PlotModel { Title = "Function Plot" };
            PlotViewControl.Model = _model;
        }

        public void PlotFunction(string function, double xMin, double xMax, double step)
        {
            _model.Series.Clear();
            var series = new LineSeries();

            if (Interpolation == InterpolationMode.Spline)
            {
                series.InterpolationAlgorithm = InterpolationAlgorithms.CanonicalSpline;
            }

            double yMin = double.PositiveInfinity;
            double yMax = double.NegativeInfinity;

            for (double x = xMin; x <= xMax; x += step)
            {
                // call F# evaluation function          
                double y = API.evaluateExpression(function, x);
                series.Points.Add(new DataPoint(x, y));

                if (y < yMin) yMin = y;
                if (y > yMax) yMax = y;
            }

            _model.Series.Add(series);

            _model.Axes.Clear();

            // X axis: use the given min/max
            var xAxis = new LinearAxis
            {
                Position = AxisPosition.Bottom,
                Minimum = xMin,
                Maximum = xMax,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineThickness = 1,
                MinorGridlineThickness = 0.5,
            };

            // If no valid points, fall back
            if (double.IsInfinity(yMin) || double.IsInfinity(yMax))
            {
                yMin = -1;
                yMax = 1;
            }

            double yRange = yMax - yMin;
            if (yRange <= 0)
            {
                yRange = 1; // flat line or single point
            }

            double padding = yRange * 0.1;

            var yAxis = new LinearAxis
            {
                Position = AxisPosition.Left,
                Minimum = yMin - padding,
                Maximum = yMax + padding,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineThickness = 1,
                MinorGridlineThickness = 0.5,
            };

            _model.Axes.Add(xAxis);
            _model.Axes.Add(yAxis);

            _model.InvalidatePlot(true); // refresh model
        }
    }
    }