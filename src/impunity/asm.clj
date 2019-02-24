(ns impunity.asm
  (:require [clojure.java.io :as io]
            [clojure.algo.monads :refer :all]           
            [impunity.utils :refer :all])
  (:import [org.objectweb.asm
            ClassReader
            MethodVisitor
            ClassVisitor
            AnnotationVisitor
            FieldVisitor
            Label
            Opcodes
            Type])
  (:import [org.objectweb.asm.tree ClassNode
            FieldNode
            MethodNode
            FieldInsnNode
            VarInsnNode           
            FrameNode
            InsnNode
            JumpInsnNode
            LabelNode
            LineNumberNode
            MethodInsnNode
            AbstractInsnNode
            LocalVariableNode
            IntInsnNode
            TypeInsnNode
            FrameNode])
  (:gen-class))


(defn access-flags [flags]
  (into #{}
        (remove false?
                [(and (not= 0 (bit-and flags Opcodes/ACC_PUBLIC))       :public)
                 (and (not= 0 (bit-and flags Opcodes/ACC_PRIVATE))      :private)
                 (and (not= 0 (bit-and flags Opcodes/ACC_PROTECTED))    :protected)
                 (and (not= 0 (bit-and flags Opcodes/ACC_STATIC))       :static)
                 (and (not= 0 (bit-and flags Opcodes/ACC_FINAL))        :final)
                 (and (not= 0 (bit-and flags Opcodes/ACC_SUPER))        :super)
                 (and (not= 0 (bit-and flags Opcodes/ACC_SYNCHRONIZED)) :synchronized)
                 (and (not= 0 (bit-and flags Opcodes/ACC_VOLATILE))     :volatile)
                 (and (not= 0 (bit-and flags Opcodes/ACC_BRIDGE))       :bridge)
                 (and (not= 0 (bit-and flags Opcodes/ACC_VARARGS))      :varargs)
                 (and (not= 0 (bit-and flags Opcodes/ACC_TRANSIENT))    :transient)
                 (and (not= 0 (bit-and flags Opcodes/ACC_NATIVE))       :native)
                 (and (not= 0 (bit-and flags Opcodes/ACC_INTERFACE))    :interface)
                 (and (not= 0 (bit-and flags Opcodes/ACC_ABSTRACT))     :abstract)
                 (and (not= 0 (bit-and flags Opcodes/ACC_STRICT))       :strict)
                 (and (not= 0 (bit-and flags Opcodes/ACC_SYNTHETIC))    :synthetic)
                 (and (not= 0 (bit-and flags Opcodes/ACC_ANNOTATION))   :annotation)
                 (and (not= 0 (bit-and flags Opcodes/ACC_ENUM))         :enum)
                 (and (not= 0 (bit-and flags Opcodes/ACC_MANDATED))     :mandated)])))


(defprotocol IParseInsn
  (parse-insn [insn]))

(defn parse-type [t]
  t)

(defn kind [kind insn]
  (with-meta insn {:kind kind}))

(extend-protocol IParseInsn
  LabelNode
  (parse-insn [insn]
    (kind :nop [:label insn]))
  
  FrameNode
  (parse-insn [insn]
    (let [type   (.-type insn)
          local  (.-local insn)
          stack  (.-stack insn)]
      (case+ type           
             Opcodes/F_NEW    (kind :nop [:frame :new    local stack]) 
             Opcodes/F_FULL   (kind :nop [:frame :full   local stack])
             Opcodes/F_APPEND (kind :nop [:frame :append local stack])
             Opcodes/F_CHOP   (kind :nop [:frame :chop   local stack])
             Opcodes/F_SAME   (kind :nop [:frame :same   local stack])           
             Opcodes/F_SAME1  (kind :nop [:frame :same1  local stack]))))

  LineNumberNode
  (parse-insn [insn]
    (kind :nop [:linenumber (.-start insn)]))
  
  IntInsnNode
  (parse-insn [insn]
    (let [operand (.operand insn)]
      (case+ (.getOpcode insn)
             Opcodes/BIPUSH   (kind :stack [:bipush   operand])             
             Opcodes/SIPUSH   (kind :stack [:sipush   operand])
             Opcodes/NEWARRAY (kind :heap [:newarray operand]))))

  TypeInsnNode
  (parse-insn [insn]
    (let [desc (.desc insn)]
      (case+ (.getOpcode insn)
             Opcodes/NEW        (kind :heap    [:new        (parse-type desc)])
             Opcodes/ANEWARRAY  (kind :heap    [:anewarray  (parse-type desc)]) 
             Opcodes/INSTANCEOF (kind :stack   [:instanceof (parse-type desc)])
             Opcodes/CHECKCAST  (kind :control [:checkcast  (parse-type desc)]))))

  FieldInsnNode
  (parse-insn [insn]
    (let [owner (.owner insn)
          name  (.name insn)
          desc  (.desc insn)]
      (case+ (.getOpcode insn)
             Opcodes/GETSTATIC (kind :heap [:getstatic owner name (parse-type desc)]) 
             Opcodes/PUTSTATIC (kind :heap [:putstatic owner name (parse-type desc)]) 
             Opcodes/GETFIELD  (kind :heap [:getfield  owner name (parse-type desc)])
             Opcodes/PUTFIELD  (kind :heap [:putfield  owner name (parse-type desc)])))) 
  
  VarInsnNode
  (parse-insn [insn]
    (let [var (.var insn)]
      (case+ (.getOpcode insn)
             Opcodes/ILOAD  (kind :heap [:load var  "I"])
             Opcodes/LLOAD  (kind :heap [:load var  "L"])
             Opcodes/FLOAD  (kind :heap [:load var  "F"])
             Opcodes/DLOAD  (kind :heap [:load var  "D"])
             Opcodes/ALOAD  (kind :heap [:load var  "A"])
             Opcodes/ISTORE (kind :heap [:store var "I"])
             Opcodes/LSTORE (kind :heap [:store var "L"])
             Opcodes/FSTORE (kind :heap [:store var "F"])
             Opcodes/DSTORE (kind :heap [:store var "D"])
             Opcodes/ASTORE (kind :heap [:store var "A"])
             Opcodes/RET    (kind :control [:ret var]))))

  MethodInsnNode 
  (parse-insn [^MethodInsnNode insn]
    (let [owner (.-owner insn)
          name  (.-name insn)
          desc  (.-desc insn)
          ret   (parse-type (.getDescriptor (Type/getReturnType desc)))
          args  (map #(parse-type (.getDescriptor %)) (Type/getArgumentTypes desc))]
      (case+ (.getOpcode insn)
             Opcodes/INVOKEVIRTUAL   (kind :control [:invoke :virtual owner name [ret args]])    
             Opcodes/INVOKESPECIAL   (kind :control [:invoke :special owner name [ret args]])    
             Opcodes/INVOKEINTERFACE (kind :control [:invoke :interface owner name [ret args]])  
             Opcodes/INVOKESTATIC    (kind :control [:invoke :static owner name [ret args]]))))

  JumpInsnNode
  (parse-insn [insn]
    (let [label (.-label ^JumpInsnNode insn)]
      (case+ (.getOpcode insn)
             Opcodes/IFEQ (kind :control [:if label :eq])
             Opcodes/IFNE (kind :control [:if label :ne])
             Opcodes/IFLT (kind :control [:if label :lt])
             Opcodes/IFGE (kind :control [:if label :ge])
             Opcodes/IFGT (kind :control [:if label :gt])
             Opcodes/IFLE (kind :control [:if label :le])

             Opcodes/IF_ICMPEQ (kind :control [:if label :cmpeq "I" ]) 
             Opcodes/IF_ICMPNE (kind :control [:if label :cmpne "I" ]) 
             Opcodes/IF_ICMPLT (kind :control [:if label :cmplt "I" ]) 
             Opcodes/IF_ICMPGE (kind :control [:if label :cmpge "I" ]) 
             Opcodes/IF_ICMPGT (kind :control [:if label :cmpgt "I" ]) 
             Opcodes/IF_ICMPLE (kind :control [:if label :cmple "I" ]) 
             Opcodes/IF_ACMPEQ (kind :control [:if label :cmpeq "A" ]) 
             Opcodes/IF_ACMPNE (kind :control [:if label :cmpne "A" ]) 

             Opcodes/GOTO (kind :control [:goto label])
             Opcodes/JSR  (kind :control [:jsr  label])

             Opcodes/IFNULL    (kind :control [:if label    :null])     
             Opcodes/IFNONNULL (kind :control [:if label :nonnull]))))
  
  InsnNode
  (parse-insn [insn]
    (case+ (.getOpcode insn)
           Opcodes/NOP         (kind :nop [:nop])
           Opcodes/ACONST_NULL (kind :stack [:const nil "A"])
           Opcodes/ICONST_M1   (kind :stack [:const -1  "I"])
           Opcodes/ICONST_0    (kind :stack [:const 0   "I"])
           Opcodes/ICONST_1    (kind :stack [:const 1   "I"])
           Opcodes/ICONST_2    (kind :stack [:const 2   "I"])
           Opcodes/ICONST_3    (kind :stack [:const 3   "I"])
           Opcodes/ICONST_4    (kind :stack [:const 4   "I"])
           Opcodes/ICONST_5    (kind :stack [:const 5   "I"])
           Opcodes/LCONST_0    (kind :stack [:const 0   "L"])
           Opcodes/LCONST_1    (kind :stack [:const 1   "L"])
           Opcodes/FCONST_0    (kind :stack [:const 0   "F"])
           Opcodes/FCONST_1    (kind :stack [:const 1   "F"])
           Opcodes/FCONST_2    (kind :stack [:const 2   "F"])
           Opcodes/DCONST_0    (kind :stack [:const 0   "D"])
           Opcodes/DCONST_1    (kind :stack [:const 1   "D"])
           
           Opcodes/IALOAD  (kind :heap [:aload  "I"])
           Opcodes/LALOAD  (kind :heap [:aload  "L"])
           Opcodes/FALOAD  (kind :heap [:aload  "F"])
           Opcodes/DALOAD  (kind :heap [:aload  "D"])
           Opcodes/AALOAD  (kind :heap [:aload  "A"])
           Opcodes/BALOAD  (kind :heap [:aload  "B"])
           Opcodes/CALOAD  (kind :heap [:aload  "C"])
           Opcodes/SALOAD  (kind :heap [:aload  "S"])
           Opcodes/IASTORE (kind :heap [:astore "I"])
           Opcodes/LASTORE (kind :heap [:astore "L"])
           Opcodes/FASTORE (kind :heap [:astore "F"])
           Opcodes/DASTORE (kind :heap [:astore "D"])
           Opcodes/AASTORE (kind :heap [:astore "A"])
           Opcodes/BASTORE (kind :heap [:astore "B"])
           Opcodes/CASTORE (kind :heap [:astore "C"])
           Opcodes/SASTORE (kind :heap [:astore "S"])
           
           Opcodes/POP     (kind :stack [:pop 1])     
           Opcodes/POP2    (kind :stack [:pop 2])    
           Opcodes/DUP     (kind :stack [:dup 0])     
           Opcodes/DUP_X1  (kind :stack [:dup 1])  
           Opcodes/DUP_X2  (kind :stack [:dup 2])  
           Opcodes/DUP2    (kind :stack [:dup2 0])    
           Opcodes/DUP2_X1 (kind :stack [:dup2 1]) 
           Opcodes/DUP2_X2 (kind :stack [:dup2 2]) 
           Opcodes/SWAP    (kind :stack [:swap])    
      
           Opcodes/IADD  (kind  :stack [:add "I"])
           Opcodes/LADD  (kind  :stack [:add "L"])
           Opcodes/FADD  (kind  :stack [:add "F"])
           Opcodes/DADD  (kind  :stack [:add "D"])
           Opcodes/ISUB  (kind  :stack [:sub "I"])
           Opcodes/LSUB  (kind  :stack [:sub "L"])
           Opcodes/FSUB  (kind  :stack [:sub "F"])
           Opcodes/DSUB  (kind  :stack [:sub "D"])
           Opcodes/IMUL  (kind  :stack [:mul "I"])
           Opcodes/LMUL  (kind  :stack [:mul "L"])
           Opcodes/FMUL  (kind  :stack [:mul "F"])
           Opcodes/DMUL  (kind  :stack [:mul "D"])
           Opcodes/IDIV  (kind  :stack [:div "I"])
           Opcodes/LDIV  (kind  :stack [:div "L"])
           Opcodes/FDIV  (kind  :stack [:div "F"])
           Opcodes/DDIV  (kind  :stack [:div "D"])
           Opcodes/IREM  (kind  :stack [:rem "I"])
           Opcodes/LREM  (kind  :stack [:rem "L"])
           Opcodes/FREM  (kind  :stack [:rem "F"])
           Opcodes/DREM  (kind  :stack [:rem "D"])
           Opcodes/INEG  (kind  :stack [:neg "I"])
           Opcodes/LNEG  (kind  :stack [:neg "L"])
           Opcodes/FNEG  (kind  :stack [:neg "F"])
           Opcodes/DNEG  (kind  :stack [:neg "D"])
           Opcodes/ISHL  (kind  :stack [:shl "I"])
           Opcodes/LSHL  (kind  :stack [:shl "L"])
           Opcodes/ISHR  (kind  :stack [:shr "I"])
           Opcodes/LSHR  (kind  :stack [:shr "L"])
           Opcodes/IUSHR (kind  :stack [:ushr "I"])
           Opcodes/LUSHR (kind  :stack [:ushr "L"])
           Opcodes/IAND  (kind  :stack [:and  "I"])
           Opcodes/LAND  (kind  :stack [:and  "L"])
           Opcodes/IOR   (kind  :stack [:or   "I"])
           Opcodes/LOR   (kind  :stack [:or   "L"])
           Opcodes/IXOR  (kind  :stack [:xor  "I"])
           Opcodes/LXOR  (kind  :stack [:xor  "L"])
      
           Opcodes/I2L (kind :stack [:2  "I" "L"])
           Opcodes/I2F (kind :stack [:2  "I" "F"])
           Opcodes/I2D (kind :stack [:2  "I" "D"])
           Opcodes/L2I (kind :stack [:2  "L" "I"])
           Opcodes/L2F (kind :stack [:2  "L" "F"])
           Opcodes/L2D (kind :stack [:2  "L" "D"])
           Opcodes/F2I (kind :stack [:2  "F" "I"])
           Opcodes/F2L (kind :stack [:2  "F" "L"])
           Opcodes/F2D (kind :stack [:2  "F" "D"])
           Opcodes/D2I (kind :stack [:2  "D" "I"])
           Opcodes/D2L (kind :stack [:2  "D" "L"])
           Opcodes/D2F (kind :stack [:2  "D" "F"])
           Opcodes/I2B (kind :stack [:2  "I" "B"])
           Opcodes/I2C (kind :stack [:2  "I" "C"])
           Opcodes/I2S (kind :stack [:2  "I" "S"])
      
           Opcodes/LCMP   (kind :stack [:cmp  "L"])      
           Opcodes/FCMPL  (kind :stack [:cmpl "F"])    
           Opcodes/FCMPG  (kind :stack [:cmpg "F"])    
           Opcodes/DCMPL  (kind :stack [:cmpl "D"])    
           Opcodes/DCMPG  (kind :stack [:cmpg "D"])   
      
           Opcodes/IRETURN (kind :control [:return  "I"]) 
           Opcodes/LRETURN (kind :control [:return  "L"]) 
           Opcodes/FRETURN (kind :control [:return  "F"]) 
           Opcodes/DRETURN (kind :control [:return  "D"]) 
           Opcodes/ARETURN (kind :control [:return  "A"]) 
           Opcodes/RETURN  (kind :control [:return  "V"])  
           
           Opcodes/ARRAYLENGTH (kind :heap    [:arraylength])  
           Opcodes/ATHROW      (kind :control [:athrow])       
           
           Opcodes/MONITORENTER (kind :control [:monitor :enter])
           Opcodes/MONITOREXIT  (kind :control [:monitor :exit]))))

(defn method-node [^MethodNode method]
  (let[access         (.access method)
       method-name    (.name method)                        
       localVariables (.localVariables method)
       method-type    (.desc method)
       ret            (parse-type (.getDescriptor (Type/getReturnType method-type)))
       args           (map #(parse-type (.getDescriptor %)) (Type/getArgumentTypes method-type))
       instructions   (->> method
                           .instructions
                           .iterator
                           iterator-seq
                           (map parse-insn))
       labels         (->> instructions
                           (keep-indexed (fn [i c]
                                           [c i]))
                           (filter (fn [[[name :as insn] i]]
                                     (= name :label)))
                           (map (fn [[[_ label] i]]
                                  [label i]))
                           (into {}))]
    #:method
    {:access       (access-flags access)
     :name         method-name
     :type         [ret args]
     :signature    (.signature method)
     :instructions (->> instructions
                        (map (fn [[name label :as insn]]
                               (if (instance? LabelNode label)
                                 (assoc insn 1 (get labels label))
                                 insn)))
                        vec)}))

(defn field-node [^FieldNode field]
  #:field
  {:name   (.name field)
   :access (access-flags (.access field))
   :type   (parse-type (.desc field))
   :signature (.signature field)
   :value  (.value field)})

(defn class-node [^ClassNode class-node]
  (let [class-name (.name class-node)
        this-type  (str "L" class-name ";")]
    #:class
    {:name    class-name
     :access  (access-flags
               (.access class-node))
     :type    this-type
     :super   (str "L" (.-superName class-node) ";")
     :interfaces (.-interfaces class-node)
     :signature  (.-signature class-node)
     :fields  (into {}
                    (for [{field-name :field/name
                           :as field}
                          (map field-node
                               (.fields class-node))]
                      [field-name field]))
     :methods (into {}
                    (for [{method-name :method/name
                           :as method}
                          (map method-node
                               (.methods class-node))]
                      [method-name method]))}))

(defn class-node-from-loader [class-name]
  (let [rdr        (ClassReader. class-name)
        classNode  (ClassNode.)]
    (.accept rdr classNode Opcodes/ASM5)
    (class-node classNode)))
