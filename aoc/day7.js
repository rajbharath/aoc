fs = require('fs');

// FileSystem module
var DIRS = "dirs";
var FILES = "files";

function FileSystem() {
  this.fileSystem = {
    name: "/",
    DIRS: [],
    FILES: []
  };
  this.traverseStack = [];

  this.findFolderAtMostSize = function (size, fs) {
    if (!fs.size) {
      return [];
    }

    return fs.DIRS.reduce((acc, dir) => {
      if (dir.size < size) {
        acc.push(dir.size);
      }
      return this.findFolderAtMostSize(size, dir).concat(acc);
    }, []);
  }

  this.findFolderAtLeastSize = function (size, fs) {
    if (!fs.size) {
      return [];
    }

    return fs.DIRS.reduce((acc, dir) => {
      if (dir.size > size) {
        acc.push(dir.size);
      }
      return this.findFolderAtLeastSize(size, dir).concat(acc);
    }, []);
  }

}

FileSystem.prototype.goToRoot = function () {
  this.traverseStack = [this.fileSystem];
}
FileSystem.prototype.goBack = function() {
  this.traverseStack.pop();
}

FileSystem.prototype.goToDir = function (dirName) {
  let dir = this.getLastElement(this.traverseStack).DIRS.find(dir => dir.name == dirName);
  this.traverseStack.push(dir);
}

FileSystem.prototype.getLastElement = function () {
  return this.traverseStack[this.traverseStack.length - 1];
}

FileSystem.prototype.addFileToCurrentDir = function(size, name) {
  return this.getLastElement().FILES.push({
    name,
    size: Number(size)
  });
}

FileSystem.prototype.cdDir = function (dirName) {
  let newDir = {
    name: dirName,
    DIRS: [],
    FILES: []
  };
  this.getLastElement().DIRS.push(newDir);
}

FileSystem.prototype.calculateSize = function () {
  function computeSize(fs) {
    if (!fs.DIRS || !fs.FILES) {
      return 0;
    }
    let dirTotal = fs.DIRS.reduce((acc, dir) => computeSize(dir) + acc, 0);
    fs["size"] = dirTotal + fs.FILES.reduce((acc, file) => acc + file.size, 0)
    return fs["size"];
  }

  return computeSize(this.fileSystem);
}

FileSystem.prototype.getDirsWithAtMostSize = function(size) {
  return this.findFolderAtMostSize(size, this.fileSystem);
}

FileSystem.prototype.getDirsWithAtLeastSize = function(size) {
  return this.findFolderAtLeastSize(size, this.fileSystem);
}

// Command Module
function Command(cmdStr) {
  this.cmdStr = cmdStr;
  this.isCdCommand = function() {
    return this.cmdStr.startsWith("$ cd");
  }
  
  this.isLsCommand = function() {
    return this.cmdStr.startsWith("$ ls");
  }
  
  this.isDir = function() {
    return this.cmdStr.startsWith("dir ");
  }

  this.execCdCommand = function (fs) {
    let arg = cmdStr.replace("$ cd", "");
    switch (arg) {
      case " /":
        return fs.goToRoot();
      case " ..":
        return fs.goBack();
      default:
        return fs.goToDir(arg.trim());
    }
  }
}

Command.prototype.exec = function (fs) {
  if (this.isCdCommand()) {
    this.execCdCommand(fs);
  } else if (this.isLsCommand()) {
    // console.log(getLastElement(traverseStack));
  } else  if (this.isDir()) {
    fs.cdDir(this.cmdStr.replace("dir ", ""));
  } else {
    fs.addFileToCurrentDir(...this.cmdStr.split(" "))
  }
}



// Application
function queryFileSystem(filename, puzzleFunction) {
  var filesys = new FileSystem();

  fs.readFile(filename, 'utf8', function (err, data) {
    if (err) {
      return console.log(err);
    }
    lines = data.split(/\r?\n/);
    lines
      .map(line => new Command(line))
      .forEach(cmd => cmd.exec(filesys));
    filesys.calculateSize();
    
    puzzleFunction(filesys);
  });
}
  
function puzzlePart1(fs) {
  console.log(fs.getDirsWithAtMostSize(100000).reduce((acc, size) => acc + size, 0));
}

function puzzlePart2(fs) {
  var totalSize = fs.calculateSize();
  var toBeFreed = 30000000 - (70000000 - totalSize);
  console.log(fs.getDirsWithAtLeastSize(toBeFreed).sort((a, b) => {
    if (a < b) { return -1 }
    if (a > b) { return 1 }
    return 0
  }));
}

// Part1
queryFileSystem("../testday7.txt", puzzlePart1);
queryFileSystem("../inputday7.txt", puzzlePart1);

// Part2
queryFileSystem("../testday7.txt", puzzlePart2);
queryFileSystem("../inputday7.txt", puzzlePart2);
