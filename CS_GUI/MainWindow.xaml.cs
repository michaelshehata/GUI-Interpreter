using System.Collections.ObjectModel;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using static API;

namespace CS_GUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public ObservableCollection<CalculationHistoryItem> History { get; }
            = new ObservableCollection<CalculationHistoryItem>();
        public MainWindow()
        {
            InitializeComponent();
            DataContext = this;
        }

        private void AddToHistory(string expression, string result)
        {
            History.Insert(0, new CalculationHistoryItem
            {
                Expression = expression,
                Result = result
            });
        }
        private void TextBox_TextChanged(object sender, TextChangedEventArgs e)
        {

        }

        private void buttonCalculate_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = ""; // clear error box text

                string inputText = inputBox.Text;
                API.clearPlotPoints();
                // send input text to interpreter
                string interpreterReturn = API.interpret(inputText);
                AddToHistory(inputText, interpreterReturn); // add expression and result to history
                var points = API.getPlotPoints();
                if (points.Any())
                {
                    int interpMode = API.getInterpolationMode();
                    PlotArea.Interpolation = interpMode == 0
                        ? PlottingArea.InterpolationMode.Linear
                        : PlottingArea.InterpolationMode.Spline;

                    double minX = points.Min(p => p.Item1);
                    double maxX = points.Max(p => p.Item1);
                    PlotArea.PlotFunction(points, minX, maxX);
                }
    

            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }

        }

        private void ClearHistoryButton_Click(object sender, RoutedEventArgs e)
        {
            History.Clear();
        }

        private void buttonPlot_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";

                string function = inputBox.Text;
                // strip leading "y ="
                string expr = GetCurrentExpression();

                double minX = MinXDoubleUpDown.Value ?? 0;
                double maxX = MaxXDoubleUpDown.Value ?? 0;
                double step = StepDoubleUpDown.Value ?? 1;

                // ask interpreter to compute points
                API.plotFunction(expr, minX, maxX, step);

                // read points from F# list
                var points = API.getPlotPoints();

                // hand points over to plotting area
                PlotArea.ResetAxes();
                PlotArea.PlotFunction(points, minX, maxX);
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }
        private string GetCurrentExpression()
        {
            string text = inputBox.Text;
            return text.Contains("=")
                ? text.Split('=')[1].Trim()
                : text.Trim();
        }

        private void buttonDerivative_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double x0 = DerivativeX0UpDown.Value ?? 0.0;
                double stepSize = 1e-4;

                double value = API.differentiateNumeric(expr, x0, stepSize);

                AddToHistory($"f'({x0}) for {expr}", value.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void buttonRoot_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double a = RootAUpDown.Value ?? 0.0;
                double b = RootBUpDown.Value ?? 0.0;

                double tolerance = 1e-6;
                int maxIterations = 100;

                double root = API.findRootBisection(expr, a, b, tolerance, maxIterations);

                AddToHistory($"Root in [{a}, {b}] for {expr}", root.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void buttonIntegrate_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = string.Empty;

                string expr = GetCurrentExpression();
                double a = IntegralAUpDown.Value ?? 0.0;
                double b = IntegralBUpDown.Value ?? 0.0;

                int steps = 1000; // fixed resolution (might change)

                double area = API.integrateTrapezoidal(expr, a, b, steps);

                AddToHistory($"∫[{a}, {b}] {expr} dx", area.ToString());
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }
        private void buttonParseTree_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";
                parseTreeBox.Text = "";

                string inputText = inputBox.Text;
                string tree = API.getParseTreeString(inputText);
                parseTreeBox.Text = tree;
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void InterpolationCombo_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (!IsLoaded) return;

            switch (InterpolationCombo.SelectedIndex)
            {
                case 0:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Linear;
                    break;
                case 1:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Spline;
                    break;
                default:
                    PlotArea.Interpolation = PlottingArea.InterpolationMode.Linear;
                    break;
            }
        }

        private void buttonPlotTangent_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";

                string expr = GetCurrentExpression();
                double x0 = DerivativeX0UpDown.Value ?? 0.0;

                double stepSize = 1e-4;

                // f(x0)
                double fx0 = API.evaluateExpression(expr, x0);

                // f'(x0)
                double slope = API.differentiateNumeric(expr, x0, stepSize);

                // plot tangent line on existing plot
                PlotArea.PlotTangentLine(
                    x0,
                    fx0,
                    slope,
                    MinXDoubleUpDown.Value ?? x0 - 5,
                    MaxXDoubleUpDown.Value ?? x0 + 5
                );
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void buttonClearTangents_Click(object sender, RoutedEventArgs e)
        {
            PlotArea.ClearTangents();
        }


        private void buttonPlotIntegral_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                errorBox.Text = "";

                string expr = GetCurrentExpression();
                double a = IntegralAUpDown.Value ?? 0.0;
                double b = IntegralBUpDown.Value ?? 0.0;

                if (a > b)
                {
                    double tmp = a;
                    a = b;
                    b = tmp;
                }

                int steps = 1000;

                // calculate area
                double area = API.integrateTrapezoidal(expr, a, b, steps);

                // show visual area
                double step = StepDoubleUpDown.Value ?? 0.01;

                API.clearPlotPoints();
                API.plotFunction(expr, a, b, step);
                var areaPoints = API.getPlotPoints();

                PlotArea.PlotIntegrationArea(
                    areaPoints,
                    a,
                    b,
                    area,
                    expr
                );
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }

        private void buttonClearIntegrals_Click(object sender, RoutedEventArgs e)
        {
            PlotArea.ClearIntegrals();
     
        }

        private void AspectRatioCheckBox_Changed(object sender, RoutedEventArgs e)
        {
            PlotArea.LockAspectRatio =
                (sender as CheckBox)?.IsChecked ?? false;

            // replot current function if possible
            try
            {
                string expr = GetCurrentExpression();

                double minX = MinXDoubleUpDown.Value ?? 0;
                double maxX = MaxXDoubleUpDown.Value ?? 0;
                double step = StepDoubleUpDown.Value ?? 0.1;

                API.clearPlotPoints();
                API.plotFunction(expr, minX, maxX, step);

                var points = API.getPlotPoints();
                PlotArea.ResetAxes();
                PlotArea.PlotFunction(points, minX, maxX);
            }
            catch
            {
                // ignore if no valid plot yet
            }
        }


    }
}