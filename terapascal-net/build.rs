use std::process::Command;

fn main() -> Result<(), i32> {
    let mut dotnet_cmd = Command::new("dotnet");
    dotnet_cmd.arg("build");

    if let Err(err) = dotnet_cmd.spawn().and_then(|mut child| child.wait()) {
        eprintln!("dotnet build failed: {err}");
        return Err(1);
    }
    
    Ok(())
}
