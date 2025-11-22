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
using OxyPlot.Series;
using OxyPlot.Wpf;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for PlottingArea.xaml
    /// </summary>
    public partial class PlottingArea : UserControl
    {
        public PlottingArea()
        {
            InitializeComponent();
            PlotViewControl.Model = new PlotModel { Title = "Function Plot" };
        }

        public void PlotFunction(string function, double xMin, double xMax, double step)
        {
            var model = new PlotModel { Title = "Function Plot" };
            var series = new LineSeries();

            for (double x = xMin; x <= xMax; x += step)
            {
                // CHANGED: Simple_Interpreter.GUIInterpret.evaluateExpression -> API.evaluateExpression
                double y = API.evaluateExpression(function, x);
                series.Points.Add(new DataPoint(x, y));
            }

            model.Series.Add(series);
            PlotViewControl.Model = model;
        }
    }
}