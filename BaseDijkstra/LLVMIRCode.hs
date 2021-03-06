module BaseDijkstra.LLVMIRCode (inputFuncs, inputFuncStrs) where

inputFuncStrs :: String
inputFuncStrs = unlines $ [
  "@.floatfmt = private unnamed_addr constant [4 x i8] c\"%lf\\00\", align 1",
  "@.intfmt = private unnamed_addr constant [4 x i8] c\"%ld\\00\", align 1",
  "@.boolfmt = private unnamed_addr constant [4 x i8] c\"%1d\\00\", align 1",
  "@.restfmt = private unnamed_addr constant [4 x i8] c\"%*s\\00\", align 1"]

inputFuncs :: String
inputFuncs = unlines $ [
  "define double @inputFloat(i8* %msg) nounwind uwtable {",
  "  entry:",
  "    %0 = alloca i8*, align 8",
  "    %var = alloca double, align 8",
  "    store i8* %msg, i8** %0, align 8",
  "    %1 = load i8** %0, align 8",
  "    br label %loop.setup",
  "  loop.setup:",
  "    call i32 (i8*, ...)* @printf(i8* %1)",
  "    %res.initial = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8]* @.floatfmt, i32 0, i32 0), double* %var)",
  "    br label %loop.start",
  "  loop.start:",
  "    %res = phi i32 [ %res.initial, %loop.setup ], [ %res.loop, %loop.body ]",
  "    %cond = icmp eq i32 %res, 0",
  "    br i1 %cond, label %loop.body, label %loop.end",
  "  loop.body:",
  "    call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8]* @.restfmt, i32 0, i32 0))",
  "    call i32 (i8*, ...)* @printf(i8* %1)",
  "    %res.loop = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8]* @.floatfmt, i32 0, i32 0), double* %var)",
  "    br label %loop.start",
  "  loop.end:",
  "    %finalVal = load double* %var, align 8",
  "    ret double %finalVal",
  "}",
  "",
  "declare i32 @printf(i8*, ...)",
  "",
  "declare i32 @__isoc99_scanf(i8*, ...)",
  "",
  "define i64 @inputInt(i8* %msg) nounwind uwtable {",
  "  entry:",
  "    %0 = alloca i8*, align 8",
  "    %var = alloca i64, align 8",
  "    store i8* %msg, i8** %0, align 8",
  "    %1 = load i8** %0, align 8",
  "    br label %loop.setup",
  "  loop.setup:",
  "    call i32 (i8*, ...)* @printf(i8* %1)",
  "    %res.initial = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8]* @.intfmt, i32 0, i32 0), i64* %var)",
  "    br label %loop.start",
  "  loop.start:",
  "    %res = phi i32 [ %res.initial, %loop.setup ], [ %res.loop, %loop.body ]",
  "    %cond = icmp eq i32 %res, 0",
  "    br i1 %cond, label %loop.body, label %loop.end",
  "  loop.body:",
  "    call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8]* @.restfmt, i32 0, i32 0))",
  "    call i32 (i8*, ...)* @printf(i8* %1)",
  "    %res.loop = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8]* @.intfmt, i32 0, i32 0), i64* %var)",
  "    br label %loop.start",
  "  loop.end:",
  "    %finalVal = load i64* %var, align 8",
  "    ret i64 %finalVal",
  "}",
  "",
  "define i1 @inputBool(i8* %msg) nounwind uwtable {",
  "  %1 = alloca i8*, align 8",
  "  %tmpv = alloca i32, align 4",
  "  store i8* %msg, i8** %1, align 8",
  "  store i32 2, i32* %tmpv, align 4",
  "  br label %2",
  "",
  "; <label>:2                                       ; preds = %5, %0",
  "  %3 = load i32* %tmpv, align 4",
  "  %4 = icmp sgt i32 %3, 1",
  "  %a4 = icmp slt i32 %3, 0",
  "  %b4 = or i1 %4, %a4",
  "  br i1 %b4, label %5, label %8",
  "",
  "; <label>:5                                       ; preds = %2",
  "  %s6 = load i8** %1, align 8",
  "  %6 = call i32 (i8*, ...)* @printf(i8* %s6)",
  "  %7 = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([4 x i8]* @.boolfmt, i32 0, i32 0), i32* %tmpv)",
  "  br label %2",
  "",
  "; <label>:8                                       ; preds = %2",
  "  %9 = load i32* %tmpv, align 4",
  "  %10 = icmp sgt i32 %9, 0",
  "  ret i1 %10",
  "}"]
