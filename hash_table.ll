; ModuleID = 'lib/hash_table.c'
source_filename = "lib/hash_table.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.hashtable_s = type { i32, i32, %struct.entry_s**, i8 }
%struct.entry_s = type { i8*, i8*, %struct.entry_s* }

@.str = private unnamed_addr constant [15 x i8] c"growing table\0A\00", align 1
@.str.1 = private unnamed_addr constant [20 x i8] c"TRYING TO FIND %lu\0A\00", align 1
@.str.2 = private unnamed_addr constant [10 x i8] c"KEY: %lu\0A\00", align 1
@.str.3 = private unnamed_addr constant [6 x i8] c"LOOP\0A\00", align 1
@.str.4 = private unnamed_addr constant [14 x i8] c"PAIRKEY: %lu\0A\00", align 1
@.str.5 = private unnamed_addr constant [15 x i8] c"FOUND NOTHING\0A\00", align 1
@.str.6 = private unnamed_addr constant [11 x i8] c"FOUND %lu\0A\00", align 1
@.str.7 = private unnamed_addr constant [6 x i8] c"(%d) \00", align 1
@.str.8 = private unnamed_addr constant [7 x i8] c"%lu : \00", align 1
@.str.9 = private unnamed_addr constant [6 x i8] c"%lu; \00", align 1
@.str.10 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.hashtable_s* @ht_create(i32 %0, i1 zeroext %1) #0 {
  %3 = alloca %struct.hashtable_s*, align 8
  %4 = alloca i32, align 4
  %5 = alloca i8, align 1
  %6 = alloca %struct.hashtable_s*, align 8
  %7 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  %8 = zext i1 %1 to i8
  store i8 %8, i8* %5, align 1
  store %struct.hashtable_s* null, %struct.hashtable_s** %6, align 8
  %9 = load i32, i32* %4, align 4
  %10 = icmp slt i32 %9, 1
  br i1 %10, label %11, label %12

11:                                               ; preds = %2
  store i32 1, i32* %4, align 4
  br label %12

12:                                               ; preds = %11, %2
  %13 = load i32, i32* %4, align 4
  %14 = mul nsw i32 %13, 2
  %15 = call i32 @find_prime(i32 %14)
  store i32 %15, i32* %4, align 4
  %16 = call i8* @malloc(i64 24) #5
  %17 = bitcast i8* %16 to %struct.hashtable_s*
  store %struct.hashtable_s* %17, %struct.hashtable_s** %6, align 8
  %18 = icmp eq %struct.hashtable_s* %17, null
  br i1 %18, label %19, label %20

19:                                               ; preds = %12
  store %struct.hashtable_s* null, %struct.hashtable_s** %3, align 8
  br label %57

20:                                               ; preds = %12
  %21 = load i32, i32* %4, align 4
  %22 = sext i32 %21 to i64
  %23 = mul i64 8, %22
  %24 = call i8* @malloc(i64 %23) #5
  %25 = bitcast i8* %24 to %struct.entry_s**
  %26 = load %struct.hashtable_s*, %struct.hashtable_s** %6, align 8
  %27 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %26, i32 0, i32 2
  store %struct.entry_s** %25, %struct.entry_s*** %27, align 8
  %28 = icmp eq %struct.entry_s** %25, null
  br i1 %28, label %29, label %30

29:                                               ; preds = %20
  store %struct.hashtable_s* null, %struct.hashtable_s** %3, align 8
  br label %57

30:                                               ; preds = %20
  store i32 0, i32* %7, align 4
  br label %31

31:                                               ; preds = %42, %30
  %32 = load i32, i32* %7, align 4
  %33 = load i32, i32* %4, align 4
  %34 = icmp slt i32 %32, %33
  br i1 %34, label %35, label %45

35:                                               ; preds = %31
  %36 = load %struct.hashtable_s*, %struct.hashtable_s** %6, align 8
  %37 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %36, i32 0, i32 2
  %38 = load %struct.entry_s**, %struct.entry_s*** %37, align 8
  %39 = load i32, i32* %7, align 4
  %40 = sext i32 %39 to i64
  %41 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %38, i64 %40
  store %struct.entry_s* null, %struct.entry_s** %41, align 8
  br label %42

42:                                               ; preds = %35
  %43 = load i32, i32* %7, align 4
  %44 = add nsw i32 %43, 1
  store i32 %44, i32* %7, align 4
  br label %31

45:                                               ; preds = %31
  %46 = load %struct.hashtable_s*, %struct.hashtable_s** %6, align 8
  %47 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %46, i32 0, i32 1
  store i32 0, i32* %47, align 4
  %48 = load i32, i32* %4, align 4
  %49 = load %struct.hashtable_s*, %struct.hashtable_s** %6, align 8
  %50 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %49, i32 0, i32 0
  store i32 %48, i32* %50, align 8
  %51 = load i8, i8* %5, align 1
  %52 = trunc i8 %51 to i1
  %53 = load %struct.hashtable_s*, %struct.hashtable_s** %6, align 8
  %54 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %53, i32 0, i32 3
  %55 = zext i1 %52 to i8
  store i8 %55, i8* %54, align 8
  %56 = load %struct.hashtable_s*, %struct.hashtable_s** %6, align 8
  store %struct.hashtable_s* %56, %struct.hashtable_s** %3, align 8
  br label %57

57:                                               ; preds = %45, %29, %19
  %58 = load %struct.hashtable_s*, %struct.hashtable_s** %3, align 8
  ret %struct.hashtable_s* %58
}

declare i32 @find_prime(i32) #1

; Function Attrs: allocsize(0)
declare i8* @malloc(i64) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.hashtable_s* @ht_grow(%struct.hashtable_s* %0) #0 {
  %2 = alloca %struct.hashtable_s*, align 8
  %3 = alloca i32, align 4
  %4 = alloca %struct.hashtable_s*, align 8
  %5 = alloca i32, align 4
  %6 = alloca %struct.entry_s*, align 8
  store %struct.hashtable_s* %0, %struct.hashtable_s** %2, align 8
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str, i64 0, i64 0))
  %8 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %9 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %8, i32 0, i32 1
  %10 = load i32, i32* %9, align 4
  store i32 %10, i32* %3, align 4
  %11 = load i32, i32* %3, align 4
  %12 = mul nsw i32 %11, 2
  %13 = call i32 @find_prime(i32 %12)
  store i32 %13, i32* %3, align 4
  %14 = load i32, i32* %3, align 4
  %15 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %16 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %15, i32 0, i32 3
  %17 = load i8, i8* %16, align 8
  %18 = trunc i8 %17 to i1
  %19 = call %struct.hashtable_s* @ht_create(i32 %14, i1 zeroext %18)
  store %struct.hashtable_s* %19, %struct.hashtable_s** %4, align 8
  store i32 0, i32* %5, align 4
  br label %20

20:                                               ; preds = %57, %1
  %21 = load i32, i32* %5, align 4
  %22 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %23 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %22, i32 0, i32 0
  %24 = load i32, i32* %23, align 8
  %25 = icmp slt i32 %21, %24
  br i1 %25, label %26, label %60

26:                                               ; preds = %20
  %27 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %28 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %27, i32 0, i32 2
  %29 = load %struct.entry_s**, %struct.entry_s*** %28, align 8
  %30 = load i32, i32* %5, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %29, i64 %31
  %33 = load %struct.entry_s*, %struct.entry_s** %32, align 8
  store %struct.entry_s* %33, %struct.entry_s** %6, align 8
  br label %34

34:                                               ; preds = %44, %26
  %35 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %36 = icmp ne %struct.entry_s* %35, null
  br i1 %36, label %37, label %42

37:                                               ; preds = %34
  %38 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %39 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %38, i32 0, i32 0
  %40 = load i8*, i8** %39, align 8
  %41 = icmp ne i8* %40, null
  br label %42

42:                                               ; preds = %37, %34
  %43 = phi i1 [ false, %34 ], [ %41, %37 ]
  br i1 %43, label %44, label %56

44:                                               ; preds = %42
  %45 = load %struct.hashtable_s*, %struct.hashtable_s** %4, align 8
  %46 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %47 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %46, i32 0, i32 0
  %48 = load i8*, i8** %47, align 8
  %49 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %50 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %49, i32 0, i32 1
  %51 = load i8*, i8** %50, align 8
  %52 = call %struct.hashtable_s* @ht_set(%struct.hashtable_s* %45, i8* %48, i8* %51)
  %53 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %54 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %53, i32 0, i32 2
  %55 = load %struct.entry_s*, %struct.entry_s** %54, align 8
  store %struct.entry_s* %55, %struct.entry_s** %6, align 8
  br label %34

56:                                               ; preds = %42
  br label %57

57:                                               ; preds = %56
  %58 = load i32, i32* %5, align 4
  %59 = add nsw i32 %58, 1
  store i32 %59, i32* %5, align 4
  br label %20

60:                                               ; preds = %20
  %61 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  call void @ht_delete(%struct.hashtable_s* %61)
  %62 = load %struct.hashtable_s*, %struct.hashtable_s** %4, align 8
  ret %struct.hashtable_s* %62
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.hashtable_s* @ht_set(%struct.hashtable_s* %0, i8* %1, i8* %2) #0 {
  %4 = alloca %struct.hashtable_s*, align 8
  %5 = alloca %struct.hashtable_s*, align 8
  %6 = alloca i8*, align 8
  %7 = alloca i8*, align 8
  %8 = alloca i32, align 4
  %9 = alloca i8, align 1
  %10 = alloca %struct.entry_s*, align 8
  %11 = alloca %struct.entry_s*, align 8
  %12 = alloca %struct.entry_s*, align 8
  %13 = alloca i8**, align 8
  %14 = alloca i8*, align 8
  %15 = alloca i8**, align 8
  %16 = alloca i8*, align 8
  %17 = alloca i8, align 1
  %18 = alloca i8, align 1
  store %struct.hashtable_s* %0, %struct.hashtable_s** %5, align 8
  store i8* %1, i8** %6, align 8
  store i8* %2, i8** %7, align 8
  store i32 0, i32* %8, align 4
  %19 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %20 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %19, i32 0, i32 3
  %21 = load i8, i8* %20, align 8
  %22 = trunc i8 %21 to i1
  %23 = zext i1 %22 to i8
  store i8 %23, i8* %9, align 1
  store %struct.entry_s* null, %struct.entry_s** %10, align 8
  store %struct.entry_s* null, %struct.entry_s** %11, align 8
  store %struct.entry_s* null, %struct.entry_s** %12, align 8
  %24 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %25 = load i8*, i8** %6, align 8
  %26 = call i32 @ht_hash(%struct.hashtable_s* %24, i8* %25)
  store i32 %26, i32* %8, align 4
  %27 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %28 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %27, i32 0, i32 2
  %29 = load %struct.entry_s**, %struct.entry_s*** %28, align 8
  %30 = load i32, i32* %8, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %29, i64 %31
  %33 = load %struct.entry_s*, %struct.entry_s** %32, align 8
  store %struct.entry_s* %33, %struct.entry_s** %11, align 8
  %34 = load i8, i8* %9, align 1
  %35 = trunc i8 %34 to i1
  br i1 %35, label %36, label %41

36:                                               ; preds = %3
  %37 = load i8*, i8** %6, align 8
  %38 = bitcast i8* %37 to i8**
  store i8** %38, i8*** %13, align 8
  %39 = load i8**, i8*** %13, align 8
  %40 = load i8*, i8** %39, align 8
  store i8* %40, i8** %6, align 8
  br label %41

41:                                               ; preds = %36, %3
  store i8 0, i8* %18, align 1
  br label %42

42:                                               ; preds = %81, %41
  %43 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %44 = icmp ne %struct.entry_s* %43, null
  br i1 %44, label %45, label %50

45:                                               ; preds = %42
  %46 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %47 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %46, i32 0, i32 0
  %48 = load i8*, i8** %47, align 8
  %49 = icmp ne i8* %48, null
  br label %50

50:                                               ; preds = %45, %42
  %51 = phi i1 [ false, %42 ], [ %49, %45 ]
  br i1 %51, label %52, label %86

52:                                               ; preds = %50
  %53 = load i8*, i8** %6, align 8
  %54 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %55 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %54, i32 0, i32 0
  %56 = load i8*, i8** %55, align 8
  %57 = call i32 @memcmp(i8* %53, i8* %56, i64 1)
  %58 = icmp eq i32 %57, 0
  %59 = zext i1 %58 to i8
  store i8 %59, i8* %17, align 1
  %60 = load i8, i8* %9, align 1
  %61 = trunc i8 %60 to i1
  br i1 %61, label %62, label %74

62:                                               ; preds = %52
  %63 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %64 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %63, i32 0, i32 0
  %65 = load i8*, i8** %64, align 8
  %66 = bitcast i8* %65 to i8**
  store i8** %66, i8*** %15, align 8
  %67 = load i8**, i8*** %15, align 8
  %68 = load i8*, i8** %67, align 8
  store i8* %68, i8** %16, align 8
  %69 = load i8*, i8** %14, align 8
  %70 = load i8*, i8** %16, align 8
  %71 = call i32 @strcmp(i8* %69, i8* %70)
  %72 = icmp eq i32 %71, 0
  %73 = zext i1 %72 to i8
  store i8 %73, i8* %18, align 1
  br label %74

74:                                               ; preds = %62, %52
  %75 = load i8, i8* %17, align 1
  %76 = trunc i8 %75 to i1
  br i1 %76, label %80, label %77

77:                                               ; preds = %74
  %78 = load i8, i8* %18, align 1
  %79 = trunc i8 %78 to i1
  br i1 %79, label %80, label %81

80:                                               ; preds = %77, %74
  br label %86

81:                                               ; preds = %77
  %82 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  store %struct.entry_s* %82, %struct.entry_s** %12, align 8
  %83 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %84 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %83, i32 0, i32 2
  %85 = load %struct.entry_s*, %struct.entry_s** %84, align 8
  store %struct.entry_s* %85, %struct.entry_s** %11, align 8
  br label %42

86:                                               ; preds = %80, %50
  %87 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %88 = icmp ne %struct.entry_s* %87, null
  br i1 %88, label %89, label %94

89:                                               ; preds = %86
  %90 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %91 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %90, i32 0, i32 0
  %92 = load i8*, i8** %91, align 8
  %93 = icmp ne i8* %92, null
  br i1 %93, label %100, label %94

94:                                               ; preds = %89, %86
  %95 = load i8, i8* %17, align 1
  %96 = trunc i8 %95 to i1
  br i1 %96, label %100, label %97

97:                                               ; preds = %94
  %98 = load i8, i8* %18, align 1
  %99 = trunc i8 %98 to i1
  br i1 %99, label %100, label %110

100:                                              ; preds = %97, %94, %89
  %101 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %102 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %101, i32 0, i32 1
  %103 = load i8*, i8** %102, align 8
  %104 = load i8*, i8** %7, align 8
  %105 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %106 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %105, i32 0, i32 1
  %107 = load i8*, i8** %106, align 8
  %108 = call i64 @llvm.objectsize.i64.p0i8(i8* %107, i1 false, i1 true, i1 false)
  %109 = call i8* @__memcpy_chk(i8* %103, i8* %104, i64 8, i64 %108) #6
  br label %158

110:                                              ; preds = %97
  %111 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %112 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %111, i32 0, i32 1
  %113 = load i32, i32* %112, align 4
  %114 = add nsw i32 %113, 1
  store i32 %114, i32* %112, align 4
  %115 = load i8*, i8** %6, align 8
  %116 = load i8*, i8** %7, align 8
  %117 = call %struct.entry_s* @ht_newpair(i8* %115, i8* %116)
  store %struct.entry_s* %117, %struct.entry_s** %10, align 8
  %118 = load %struct.entry_s*, %struct.entry_s** %10, align 8
  %119 = icmp eq %struct.entry_s* %118, null
  br i1 %119, label %120, label %121

120:                                              ; preds = %110
  store %struct.hashtable_s* null, %struct.hashtable_s** %4, align 8
  br label %175

121:                                              ; preds = %110
  %122 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %123 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %124 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %123, i32 0, i32 2
  %125 = load %struct.entry_s**, %struct.entry_s*** %124, align 8
  %126 = load i32, i32* %8, align 4
  %127 = sext i32 %126 to i64
  %128 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %125, i64 %127
  %129 = load %struct.entry_s*, %struct.entry_s** %128, align 8
  %130 = icmp eq %struct.entry_s* %122, %129
  br i1 %130, label %131, label %142

131:                                              ; preds = %121
  %132 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %133 = load %struct.entry_s*, %struct.entry_s** %10, align 8
  %134 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %133, i32 0, i32 2
  store %struct.entry_s* %132, %struct.entry_s** %134, align 8
  %135 = load %struct.entry_s*, %struct.entry_s** %10, align 8
  %136 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %137 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %136, i32 0, i32 2
  %138 = load %struct.entry_s**, %struct.entry_s*** %137, align 8
  %139 = load i32, i32* %8, align 4
  %140 = sext i32 %139 to i64
  %141 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %138, i64 %140
  store %struct.entry_s* %135, %struct.entry_s** %141, align 8
  br label %157

142:                                              ; preds = %121
  %143 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %144 = icmp eq %struct.entry_s* %143, null
  br i1 %144, label %145, label %149

145:                                              ; preds = %142
  %146 = load %struct.entry_s*, %struct.entry_s** %10, align 8
  %147 = load %struct.entry_s*, %struct.entry_s** %12, align 8
  %148 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %147, i32 0, i32 2
  store %struct.entry_s* %146, %struct.entry_s** %148, align 8
  br label %156

149:                                              ; preds = %142
  %150 = load %struct.entry_s*, %struct.entry_s** %11, align 8
  %151 = load %struct.entry_s*, %struct.entry_s** %10, align 8
  %152 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %151, i32 0, i32 2
  store %struct.entry_s* %150, %struct.entry_s** %152, align 8
  %153 = load %struct.entry_s*, %struct.entry_s** %10, align 8
  %154 = load %struct.entry_s*, %struct.entry_s** %12, align 8
  %155 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %154, i32 0, i32 2
  store %struct.entry_s* %153, %struct.entry_s** %155, align 8
  br label %156

156:                                              ; preds = %149, %145
  br label %157

157:                                              ; preds = %156, %131
  br label %158

158:                                              ; preds = %157, %100
  %159 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %160 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %159, i32 0, i32 1
  %161 = load i32, i32* %160, align 4
  %162 = sitofp i32 %161 to double
  %163 = fmul double %162, 1.300000e+00
  %164 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %165 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %164, i32 0, i32 0
  %166 = load i32, i32* %165, align 8
  %167 = sitofp i32 %166 to float
  %168 = fpext float %167 to double
  %169 = fcmp ogt double %163, %168
  br i1 %169, label %170, label %173

170:                                              ; preds = %158
  %171 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  %172 = call %struct.hashtable_s* @ht_grow(%struct.hashtable_s* %171)
  store %struct.hashtable_s* %172, %struct.hashtable_s** %5, align 8
  br label %173

173:                                              ; preds = %170, %158
  %174 = load %struct.hashtable_s*, %struct.hashtable_s** %5, align 8
  store %struct.hashtable_s* %174, %struct.hashtable_s** %4, align 8
  br label %175

175:                                              ; preds = %173, %120
  %176 = load %struct.hashtable_s*, %struct.hashtable_s** %4, align 8
  ret %struct.hashtable_s* %176
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @ht_delete(%struct.hashtable_s* %0) #0 {
  %2 = alloca %struct.hashtable_s*, align 8
  %3 = alloca i32, align 4
  %4 = alloca %struct.entry_s*, align 8
  %5 = alloca %struct.entry_s*, align 8
  store %struct.hashtable_s* %0, %struct.hashtable_s** %2, align 8
  store i32 0, i32* %3, align 4
  br label %6

6:                                                ; preds = %49, %1
  %7 = load i32, i32* %3, align 4
  %8 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %9 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %8, i32 0, i32 0
  %10 = load i32, i32* %9, align 8
  %11 = icmp slt i32 %7, %10
  br i1 %11, label %12, label %52

12:                                               ; preds = %6
  %13 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %14 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %13, i32 0, i32 2
  %15 = load %struct.entry_s**, %struct.entry_s*** %14, align 8
  %16 = load i32, i32* %3, align 4
  %17 = sext i32 %16 to i64
  %18 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %15, i64 %17
  %19 = load %struct.entry_s*, %struct.entry_s** %18, align 8
  store %struct.entry_s* %19, %struct.entry_s** %4, align 8
  br label %20

20:                                               ; preds = %41, %12
  %21 = load %struct.entry_s*, %struct.entry_s** %4, align 8
  %22 = icmp ne %struct.entry_s* %21, null
  br i1 %22, label %23, label %48

23:                                               ; preds = %20
  %24 = load %struct.entry_s*, %struct.entry_s** %4, align 8
  %25 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %24, i32 0, i32 0
  %26 = load i8*, i8** %25, align 8
  %27 = icmp ne i8* %26, null
  br i1 %27, label %28, label %32

28:                                               ; preds = %23
  %29 = load %struct.entry_s*, %struct.entry_s** %4, align 8
  %30 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %29, i32 0, i32 0
  %31 = load i8*, i8** %30, align 8
  call void @free(i8* %31)
  br label %32

32:                                               ; preds = %28, %23
  %33 = load %struct.entry_s*, %struct.entry_s** %4, align 8
  %34 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %33, i32 0, i32 1
  %35 = load i8*, i8** %34, align 8
  %36 = icmp ne i8* %35, null
  br i1 %36, label %37, label %41

37:                                               ; preds = %32
  %38 = load %struct.entry_s*, %struct.entry_s** %4, align 8
  %39 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %38, i32 0, i32 1
  %40 = load i8*, i8** %39, align 8
  call void @free(i8* %40)
  br label %41

41:                                               ; preds = %37, %32
  %42 = load %struct.entry_s*, %struct.entry_s** %4, align 8
  store %struct.entry_s* %42, %struct.entry_s** %5, align 8
  %43 = load %struct.entry_s*, %struct.entry_s** %4, align 8
  %44 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %43, i32 0, i32 2
  %45 = load %struct.entry_s*, %struct.entry_s** %44, align 8
  store %struct.entry_s* %45, %struct.entry_s** %4, align 8
  %46 = load %struct.entry_s*, %struct.entry_s** %5, align 8
  %47 = bitcast %struct.entry_s* %46 to i8*
  call void @free(i8* %47)
  br label %20

48:                                               ; preds = %20
  br label %49

49:                                               ; preds = %48
  %50 = load i32, i32* %3, align 4
  %51 = add nsw i32 %50, 1
  store i32 %51, i32* %3, align 4
  br label %6

52:                                               ; preds = %6
  %53 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %54 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %53, i32 0, i32 2
  %55 = load %struct.entry_s**, %struct.entry_s*** %54, align 8
  %56 = bitcast %struct.entry_s** %55 to i8*
  call void @free(i8* %56)
  %57 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %58 = bitcast %struct.hashtable_s* %57 to i8*
  call void @free(i8* %58)
  ret void
}

declare void @free(i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @ht_hash(%struct.hashtable_s* %0, i8* %1) #0 {
  %3 = alloca %struct.hashtable_s*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i32, align 4
  store %struct.hashtable_s* %0, %struct.hashtable_s** %3, align 8
  store i8* %1, i8** %4, align 8
  store i64 5381, i64* %5, align 8
  store i32 0, i32* %6, align 4
  br label %7

7:                                                ; preds = %18, %2
  %8 = load i64, i64* %5, align 8
  %9 = icmp ult i64 %8, -1
  br i1 %9, label %10, label %16

10:                                               ; preds = %7
  %11 = load i32, i32* %6, align 4
  %12 = sext i32 %11 to i64
  %13 = load i8*, i8** %4, align 8
  %14 = call i64 @strlen(i8* %13)
  %15 = icmp ult i64 %12, %14
  br label %16

16:                                               ; preds = %10, %7
  %17 = phi i1 [ false, %7 ], [ %15, %10 ]
  br i1 %17, label %18, label %31

18:                                               ; preds = %16
  %19 = load i64, i64* %5, align 8
  %20 = shl i64 %19, 5
  store i64 %20, i64* %5, align 8
  %21 = load i8*, i8** %4, align 8
  %22 = load i32, i32* %6, align 4
  %23 = sext i32 %22 to i64
  %24 = getelementptr inbounds i8, i8* %21, i64 %23
  %25 = load i8, i8* %24, align 1
  %26 = sext i8 %25 to i64
  %27 = load i64, i64* %5, align 8
  %28 = add i64 %27, %26
  store i64 %28, i64* %5, align 8
  %29 = load i32, i32* %6, align 4
  %30 = add nsw i32 %29, 1
  store i32 %30, i32* %6, align 4
  br label %7

31:                                               ; preds = %16
  %32 = load i64, i64* %5, align 8
  %33 = load %struct.hashtable_s*, %struct.hashtable_s** %3, align 8
  %34 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %33, i32 0, i32 0
  %35 = load i32, i32* %34, align 8
  %36 = sext i32 %35 to i64
  %37 = urem i64 %32, %36
  %38 = trunc i64 %37 to i32
  ret i32 %38
}

declare i64 @strlen(i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.entry_s* @ht_newpair(i8* %0, i8* %1) #0 {
  %3 = alloca %struct.entry_s*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca %struct.entry_s*, align 8
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  %7 = call i8* @malloc(i64 24) #5
  %8 = bitcast i8* %7 to %struct.entry_s*
  store %struct.entry_s* %8, %struct.entry_s** %6, align 8
  %9 = icmp eq %struct.entry_s* %8, null
  br i1 %9, label %10, label %11

10:                                               ; preds = %2
  store %struct.entry_s* null, %struct.entry_s** %3, align 8
  br label %45

11:                                               ; preds = %2
  %12 = call i8* @malloc(i64 8) #5
  %13 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %14 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %13, i32 0, i32 0
  store i8* %12, i8** %14, align 8
  %15 = icmp eq i8* %12, null
  br i1 %15, label %16, label %17

16:                                               ; preds = %11
  store %struct.entry_s* null, %struct.entry_s** %3, align 8
  br label %45

17:                                               ; preds = %11
  %18 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %19 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = load i8*, i8** %4, align 8
  %22 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %23 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %22, i32 0, i32 0
  %24 = load i8*, i8** %23, align 8
  %25 = call i64 @llvm.objectsize.i64.p0i8(i8* %24, i1 false, i1 true, i1 false)
  %26 = call i8* @__memcpy_chk(i8* %20, i8* %21, i64 8, i64 %25) #6
  %27 = call i8* @malloc(i64 8) #5
  %28 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %29 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %28, i32 0, i32 1
  store i8* %27, i8** %29, align 8
  %30 = icmp eq i8* %27, null
  br i1 %30, label %31, label %32

31:                                               ; preds = %17
  store %struct.entry_s* null, %struct.entry_s** %3, align 8
  br label %45

32:                                               ; preds = %17
  %33 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %34 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %33, i32 0, i32 1
  %35 = load i8*, i8** %34, align 8
  %36 = load i8*, i8** %5, align 8
  %37 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %38 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %37, i32 0, i32 1
  %39 = load i8*, i8** %38, align 8
  %40 = call i64 @llvm.objectsize.i64.p0i8(i8* %39, i1 false, i1 true, i1 false)
  %41 = call i8* @__memcpy_chk(i8* %35, i8* %36, i64 8, i64 %40) #6
  %42 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  %43 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %42, i32 0, i32 2
  store %struct.entry_s* null, %struct.entry_s** %43, align 8
  %44 = load %struct.entry_s*, %struct.entry_s** %6, align 8
  store %struct.entry_s* %44, %struct.entry_s** %3, align 8
  br label %45

45:                                               ; preds = %32, %31, %16, %10
  %46 = load %struct.entry_s*, %struct.entry_s** %3, align 8
  ret %struct.entry_s* %46
}

; Function Attrs: nounwind
declare i8* @__memcpy_chk(i8*, i8*, i64, i64) #3

; Function Attrs: nounwind readnone speculatable willreturn
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1 immarg, i1 immarg, i1 immarg) #4

declare i32 @memcmp(i8*, i8*, i64) #1

declare i32 @strcmp(i8*, i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @ht_get(%struct.hashtable_s* %0, i8* %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca %struct.hashtable_s*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i8, align 1
  %8 = alloca %struct.entry_s*, align 8
  %9 = alloca i8**, align 8
  %10 = alloca i8*, align 8
  %11 = alloca i8**, align 8
  %12 = alloca i8*, align 8
  %13 = alloca i8, align 1
  %14 = alloca i8, align 1
  store %struct.hashtable_s* %0, %struct.hashtable_s** %4, align 8
  store i8* %1, i8** %5, align 8
  %15 = load i8*, i8** %5, align 8
  %16 = ptrtoint i8* %15 to i64
  %17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.1, i64 0, i64 0), i64 %16)
  store i32 0, i32* %6, align 4
  %18 = load %struct.hashtable_s*, %struct.hashtable_s** %4, align 8
  %19 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %18, i32 0, i32 3
  %20 = load i8, i8* %19, align 8
  %21 = trunc i8 %20 to i1
  %22 = zext i1 %21 to i8
  store i8 %22, i8* %7, align 1
  %23 = load %struct.hashtable_s*, %struct.hashtable_s** %4, align 8
  %24 = load i8*, i8** %5, align 8
  %25 = call i32 @ht_hash(%struct.hashtable_s* %23, i8* %24)
  store i32 %25, i32* %6, align 4
  %26 = load %struct.hashtable_s*, %struct.hashtable_s** %4, align 8
  %27 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %26, i32 0, i32 2
  %28 = load %struct.entry_s**, %struct.entry_s*** %27, align 8
  %29 = load i32, i32* %6, align 4
  %30 = sext i32 %29 to i64
  %31 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %28, i64 %30
  %32 = load %struct.entry_s*, %struct.entry_s** %31, align 8
  store %struct.entry_s* %32, %struct.entry_s** %8, align 8
  %33 = load i8, i8* %7, align 1
  %34 = trunc i8 %33 to i1
  br i1 %34, label %35, label %43

35:                                               ; preds = %2
  %36 = load i8*, i8** %5, align 8
  %37 = bitcast i8* %36 to i8**
  store i8** %37, i8*** %9, align 8
  %38 = load i8**, i8*** %9, align 8
  %39 = load i8*, i8** %38, align 8
  store i8* %39, i8** %10, align 8
  %40 = load i8*, i8** %10, align 8
  %41 = ptrtoint i8* %40 to i64
  %42 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.2, i64 0, i64 0), i64 %41)
  br label %43

43:                                               ; preds = %35, %2
  store i8 0, i8* %14, align 1
  br label %44

44:                                               ; preds = %87, %43
  %45 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %46 = icmp ne %struct.entry_s* %45, null
  br i1 %46, label %47, label %52

47:                                               ; preds = %44
  %48 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %49 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %48, i32 0, i32 0
  %50 = load i8*, i8** %49, align 8
  %51 = icmp ne i8* %50, null
  br label %52

52:                                               ; preds = %47, %44
  %53 = phi i1 [ false, %44 ], [ %51, %47 ]
  br i1 %53, label %54, label %91

54:                                               ; preds = %52
  %55 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.3, i64 0, i64 0))
  %56 = load i8, i8* %7, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %73

58:                                               ; preds = %54
  %59 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %60 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %59, i32 0, i32 0
  %61 = load i8*, i8** %60, align 8
  %62 = bitcast i8* %61 to i8**
  store i8** %62, i8*** %11, align 8
  %63 = load i8**, i8*** %11, align 8
  %64 = load i8*, i8** %63, align 8
  store i8* %64, i8** %12, align 8
  %65 = load i8*, i8** %12, align 8
  %66 = ptrtoint i8* %65 to i64
  %67 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.4, i64 0, i64 0), i64 %66)
  %68 = load i8*, i8** %10, align 8
  %69 = load i8*, i8** %12, align 8
  %70 = call i32 @strcmp(i8* %68, i8* %69)
  %71 = icmp eq i32 %70, 0
  %72 = zext i1 %71 to i8
  store i8 %72, i8* %14, align 1
  br label %73

73:                                               ; preds = %58, %54
  %74 = load i8*, i8** %5, align 8
  %75 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %76 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %75, i32 0, i32 0
  %77 = load i8*, i8** %76, align 8
  %78 = call i32 @memcmp(i8* %74, i8* %77, i64 1)
  %79 = icmp eq i32 %78, 0
  %80 = zext i1 %79 to i8
  store i8 %80, i8* %13, align 1
  %81 = load i8, i8* %13, align 1
  %82 = trunc i8 %81 to i1
  br i1 %82, label %86, label %83

83:                                               ; preds = %73
  %84 = load i8, i8* %14, align 1
  %85 = trunc i8 %84 to i1
  br i1 %85, label %86, label %87

86:                                               ; preds = %83, %73
  br label %91

87:                                               ; preds = %83
  %88 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %89 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %88, i32 0, i32 2
  %90 = load %struct.entry_s*, %struct.entry_s** %89, align 8
  store %struct.entry_s* %90, %struct.entry_s** %8, align 8
  br label %44

91:                                               ; preds = %86, %52
  %92 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %93 = icmp eq %struct.entry_s* %92, null
  br i1 %93, label %105, label %94

94:                                               ; preds = %91
  %95 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %96 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %95, i32 0, i32 0
  %97 = load i8*, i8** %96, align 8
  %98 = icmp eq i8* %97, null
  br i1 %98, label %105, label %99

99:                                               ; preds = %94
  %100 = load i8, i8* %13, align 1
  %101 = trunc i8 %100 to i1
  br i1 %101, label %107, label %102

102:                                              ; preds = %99
  %103 = load i8, i8* %14, align 1
  %104 = trunc i8 %103 to i1
  br i1 %104, label %107, label %105

105:                                              ; preds = %102, %94, %91
  %106 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.5, i64 0, i64 0))
  store i8* null, i8** %3, align 8
  br label %116

107:                                              ; preds = %102, %99
  %108 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %109 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %108, i32 0, i32 1
  %110 = load i8*, i8** %109, align 8
  %111 = ptrtoint i8* %110 to i64
  %112 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.6, i64 0, i64 0), i64 %111)
  %113 = load %struct.entry_s*, %struct.entry_s** %8, align 8
  %114 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %113, i32 0, i32 1
  %115 = load i8*, i8** %114, align 8
  store i8* %115, i8** %3, align 8
  br label %116

116:                                              ; preds = %107, %105
  %117 = load i8*, i8** %3, align 8
  ret i8* %117
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @ht_print(%struct.hashtable_s* %0) #0 {
  %2 = alloca %struct.hashtable_s*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca %struct.entry_s*, align 8
  store %struct.hashtable_s* %0, %struct.hashtable_s** %2, align 8
  store i32 0, i32* %3, align 4
  store i32 0, i32* %4, align 4
  br label %6

6:                                                ; preds = %41, %1
  %7 = load i32, i32* %4, align 4
  %8 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %9 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %8, i32 0, i32 0
  %10 = load i32, i32* %9, align 8
  %11 = icmp slt i32 %7, %10
  br i1 %11, label %12, label %44

12:                                               ; preds = %6
  %13 = load %struct.hashtable_s*, %struct.hashtable_s** %2, align 8
  %14 = getelementptr inbounds %struct.hashtable_s, %struct.hashtable_s* %13, i32 0, i32 2
  %15 = load %struct.entry_s**, %struct.entry_s*** %14, align 8
  %16 = load i32, i32* %4, align 4
  %17 = sext i32 %16 to i64
  %18 = getelementptr inbounds %struct.entry_s*, %struct.entry_s** %15, i64 %17
  %19 = load %struct.entry_s*, %struct.entry_s** %18, align 8
  store %struct.entry_s* %19, %struct.entry_s** %5, align 8
  %20 = load i32, i32* %4, align 4
  %21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.7, i64 0, i64 0), i32 %20)
  br label %22

22:                                               ; preds = %25, %12
  %23 = load %struct.entry_s*, %struct.entry_s** %5, align 8
  %24 = icmp ne %struct.entry_s* %23, null
  br i1 %24, label %25, label %39

25:                                               ; preds = %22
  %26 = load %struct.entry_s*, %struct.entry_s** %5, align 8
  %27 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %26, i32 0, i32 0
  %28 = load i8*, i8** %27, align 8
  %29 = ptrtoint i8* %28 to i64
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.8, i64 0, i64 0), i64 %29)
  %31 = load %struct.entry_s*, %struct.entry_s** %5, align 8
  %32 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %31, i32 0, i32 1
  %33 = load i8*, i8** %32, align 8
  %34 = ptrtoint i8* %33 to i64
  %35 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.9, i64 0, i64 0), i64 %34)
  %36 = load %struct.entry_s*, %struct.entry_s** %5, align 8
  %37 = getelementptr inbounds %struct.entry_s, %struct.entry_s* %36, i32 0, i32 2
  %38 = load %struct.entry_s*, %struct.entry_s** %37, align 8
  store %struct.entry_s* %38, %struct.entry_s** %5, align 8
  br label %22

39:                                               ; preds = %22
  %40 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.10, i64 0, i64 0))
  br label %41

41:                                               ; preds = %39
  %42 = load i32, i32* %4, align 4
  %43 = add nsw i32 %42, 1
  store i32 %43, i32* %4, align 4
  br label %6

44:                                               ; preds = %6
  %45 = load i32, i32* %3, align 4
  ret i32 %45
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readnone speculatable willreturn }
attributes #5 = { allocsize(0) }
attributes #6 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 11.1.0"}
