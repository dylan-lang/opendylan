module: remake-dylan-app

define argument-parser <remake-dylan-app-argparser> ()
  synopsis print-synopsis,
    usage: "remake-dylan-app [options] [files...]",
    description: "remake-dylan-app is a tool to create new Dylan projects.";
  
  regular-arguments file-paths;
end argument-parser <remake-dylan-app-argparser>;
