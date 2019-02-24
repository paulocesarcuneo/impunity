(ns impunity.visitor
  (:import [org.objectweb.asm ClassReader
            MethodVisitor
            ClassVisitor
            AnnotationVisitor
            FieldVisitor
            Label
            Opcodes])
  (:gen-class))

(def API_VERSION Opcodes/ASM5)

(defn annotation-visitor []
  (proxy [AnnotationVisitor] [API_VERSION]
    #_void (visit [name value])
    #_AnnotationVisitor (visitAnnotation [name desc])
    #_AnnotationVisitor (visitArray [name])
    #_void (visitEnd [])
    #_void (visitEnum [name desc value])))

(defn method-visitor []
  (proxy [MethodVisitor] [API_VERSION]
    #_void (visitAttribute [attr])
    #_void (visitCode [])
    #_void (visitEnd [])
    #_void (visitFieldInsn [opcode owner name desc])
    #_void (visitFrame [type nLocal local nStack stack])
    #_void (visitIincInsn [var increment])
    #_void (visitInsn [opcode])    
    #_void (visitIntInsn [opcode operand])
    #_void (visitInvokeDynamicInsn [name desc bsm bsmArgs])
    #_void (visitJumpInsn [opcode label])
    #_void (visitLabel [label])
    #_void (visitLdcInsn [cst])
    #_void (visitLineNumber [line start])
    #_void (visitLocalVariable [name desc signature start end index])   
    #_void (visitLookupSwitchInsn [dflt keys labels])
    #_void (visitMaxs [maxStack maxLocals])
    #_void (visitMethodInsn
             ([opcode owner name desc])
             ([opcode owner name desc itf]))
    #_void (visitMultiANewArrayInsn [desc dims])
    #_void (visitParameter [name access])
    #_void (visitTryCatchBlock [start end handler type])
    #_void (visitTableSwitchInsn [min max dflt labels])
    #_void (visitTypeInsn [opcode type])
    #_void (visitVarInsn [opcode var])
    #_AnnotationVisitor (visitAnnotation [desc visible])
    #_AnnotationVisitor (visitAnnotationDefault [])
    #_AnnotationVisitor (visitParameterAnnotation [parameter desc visible])
    #_AnnotationVisitor (visitTryCatchAnnotation [typeRef typePath desc visible])
    #_AnnotationVisitor (visitTypeAnnotation [typeRef typePath desc visible])
    #_AnnotationVisitor (visitInsnAnnotation [typeRef typePath desc visible])
    #_AnnotationVisitor (visitLocalVariableAnnotation [typeRef typePath start end index desc visible])
    ))

(defn field-visitor []
  (proxy [FieldVisitor] [API_VERSION]
    #_AnnotationVisitor (visitAnnotation [desc visible])
    #_void (visitAttribute [attr])
    #_void (visitEnd [])
    #_AnnotationVisitor (visitTypeAnnotation [typeRef typePath desc visible])))

(defn class-visitor []
  (proxy [ClassVisitor] [API_VERSION]
    #_void (visit [version access name signature superName rfaces])
    #_AnnotationVisitor (visitAnnotation [desc visible]
                          (annotation-visitor))
    #_void (visitAttribute [attr])
    #_void (visitEnd [])
    #_FieldVisitor (visitField [access name desc signature Object value])
    #_void (visitInnerClass [ name outerName innerName access])
    #_MethodVisitor (visitMethod [access name desc signature exceptions]
                      (method-visitor))
    #_void (visitOuterClass [ owner name desc])
    #_void (visitSource [ source debug])
    #_AnnotationVisitor (visitTypeAnnotation [typeRef desc visible])))



