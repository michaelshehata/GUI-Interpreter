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

using static API;  // or just: using API;

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
                // CHANGED: Simple_Interpreter.GUIInterpret.interpret -> API.interpret
                string interpreterReturn = API.interpret(inputText);
                AddToHistory(inputText, interpreterReturn); // add expression and result to history

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
                string expr = function.Contains("=")
                    ? function.Split("=")[1].Trim()
                    : function.Trim();

                double minX = MinXDoubleUpDown.Value ?? 0;
                double maxX = MaxXDoubleUpDown.Value ?? 0;
                double step = StepDoubleUpDown.Value ?? 1;
                PlotArea.PlotFunction(expr, minX, maxX, step);
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
                // CHANGED: Simple_Interpreter.GUIInterpret.getParseTreeString -> API.getParseTreeString
                string tree = API.getParseTreeString(inputText);
                parseTreeBox.Text = tree;
            }
            catch (Exception ex)
            {
                errorBox.Text = ex.Message;
            }
        }
    }
}