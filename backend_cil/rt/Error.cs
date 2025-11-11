namespace Terapascal.Runtime;

public class Error : Exception {
    public Error(string message) : base($"Runtime error raised: {message}") {
    }
}
