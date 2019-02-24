

class Dep {
    public void doStuff() {

    }
}

class In {
    private int a;
    public int getA(){return this.a;}
    public void setA(int a){this.a = a;}
}

class Out {
    private int b;
    public Out(int b){ this.b=b;}
}
public class Domain {

    private Dep dep;

    public Domain(Dep dep) {
	this.dep = dep;
    }

    public Out complexLogic(In in) {
	try {
	if(in.getA() > 100) {
	    dep.doStuff();
	    return new Out(in.getA());
	}
	return new Out(0);
	}catch (Exception e) {
	    return null;
	}
    }
}
