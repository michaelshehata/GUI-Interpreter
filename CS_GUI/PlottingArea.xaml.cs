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
        public PlottingArea()
        {
            InitializeComponent();
            _model = new PlotModel { Title = "Function Plot" };

            // x axis
            _model.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Bottom,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineThickness = 1,
                MinorGridlineThickness = 0.5
            });

            // y axis
            _model.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Left,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineThickness = 1,
                MinorGridlineThickness = 0.5
            });

            PlotViewControl.Model = _model;
        }

        public void PlotFunction(string function, double xMin, double xMax, double step)
        {
            _model.Series.Clear();
            var series = new LineSeries();

            for (double x = xMin; x <= xMax; x += step)
            {
                // call F# evalution function          
                double y = API.evaluateExpression(function, x);
                series.Points.Add(new DataPoint(x, y));
            }

            _model.Series.Add(series);
            _model.InvalidatePlot(true); // refresh model
        }
    }
}