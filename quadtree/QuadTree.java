import java.util.Vector;
import java.util.Map;
import java.util.Random;

/*************************************************************************
 *  Compilation:  javac QuadTree.java
 *  Execution:    java QuadTree M N
 *
 *  Quad tree.
 * 
 *************************************************************************/

public class QuadTree  {
    private Quad mrootquad;

	private class BBObject {
        double mxmin, mymin, mxmax, mymax;
		Object mobject;
        BBObject(double xmin, double ymin, double xmax, double ymax, Object object) {
            this.mxmin = xmin;
            this.mymin = ymin;
            this.mxmax = xmax;
            this.mymax = ymax;
			this.mobject = object;
		}

		public Boolean intersect(double xmin, double ymin, double xmax, double ymax) 
		{
			Boolean result = false;
			if ( (xmin <= mxmax) && (xmax >= mxmin) && (ymin <= mymax) && (ymax >= mymin)   )
				result = true;
			return result;
		}
	}

	// helper node data type
	private class Quad {
		double mxmin, mymin, mxmax, mymax;
		Vector<Quad> msubquads;
		Vector<BBObject> mbbobjects;           // associated data

		Quad(double xmin, double ymin, double xmax, double ymax) {
			this.mxmin = xmin;
			this.mymin = ymin;
			this.mxmax = xmax;
			this.mymax = ymax;
			this.msubquads = new Vector<Quad>();
			this.mbbobjects = new Vector<BBObject>();
		}

		public Boolean intersect(double xmin, double ymin, double xmax, double ymax) 
		{
			Boolean result = false;
			if ( (xmin <= mxmax) && (xmax >= mxmin) && (ymin <= mymax) && (ymax >= mymin)   )
				result = true;
			return result;
		}

		private Boolean intersect(BBObject bbobject) 
		{
			return intersect(bbobject.mxmin, bbobject.mymin, bbobject.mxmax, bbobject.mymax);
		}


		public void add(double xmin, double ymin, double xmax, double ymax, Object object, int push)
		{
			BBObject bbobject = new BBObject(xmin, ymin, xmax, ymax, object);
			if (!intersect(xmin,ymin,xmax,ymax)) // only for root quad
			{
				mxmin = mxmin >= xmin ? xmin : mxmin;
				mxmax = mxmax <= xmax ? xmax : mxmax;
				mymin = mymin >= ymin ? ymin : mymin;
				mymax = mymax <= ymax ? ymax : mymax;

				insert( bbobject, push );
			}
			else
			{
				addwithoutcheck( bbobject, push );
			}
		}

	
		private void addwithoutcheck( BBObject bbobject, int push )
		{
			if ( msubquads.size() > 0 )
			{
				dispatch( bbobject, push );
			}
			else
			{
				insert( bbobject, push );
			}
		}


		private void dispatch( BBObject bbobject, int push )
		{
			for (Quad subquad : msubquads)
			{
				if(subquad.intersect( bbobject ))
				{
					subquad.addwithoutcheck( bbobject, push );
				}
			}
		}

		private int nobjects()
		{
			return mbbobjects.size();
		}

		private void insert( BBObject bbobject, int push )
		{
			mbbobjects.add( bbobject );

			if (nobjects() > 10)
			{
				split();
			}
		}

		private void split()
		{
			double middlex = (mxmin + mxmax)/2.0;
			double middley = (mymin + mymax)/2.0;

			msubquads.add( new Quad( mxmin, mymin, middlex, middley ) );
			msubquads.add( new Quad( mxmin, middley, middlex, mymax ) );
			msubquads.add( new Quad( middlex, middley, mxmax, mymax ) );
			msubquads.add( new Quad( middlex, mymin, mxmax, middley ) );

			for (BBObject bbobject : mbbobjects )
			{
				dispatch( bbobject, 0 );
			}
			mbbobjects.clear();
		}

		public Vector<Object> getIntersected( double xmin, double ymin, double xmax, double ymax )
		{
			Vector<Object> result = new Vector<Object>();
			if (intersect( xmin, ymin, xmax, ymax ))
			{
				if ( msubquads.size() > 0 )
				{
					for (Quad subquad : msubquads)
					{
						if (subquad.intersect( xmin, ymin, xmax, ymax  ) )
						{
							Vector<Object> subresult = subquad.getIntersected( xmin, ymin, xmax, ymax );
							result.addAll( subresult );
						}
					}
				}
				else
				{
					result.clear();
					for (BBObject bbobject : mbbobjects )
					{
						if (bbobject.intersect(xmin,ymin,xmax,ymax))
						{
							result.add( bbobject.mobject );
						}
					}
				}
			}
		
			return result;
		}
	}

	public QuadTree()
	{
		mrootquad = new Quad(-10000.0,-10000.0, 10000.0,10000.0);
	}

	public void add(double xmin, double ymin, double xmax, double ymax, Object object)
	{
		mrootquad.add(xmin, ymin, xmax, ymax, object, 0 );
	}

    public Boolean iscolliding(double xmin, double ymin, double xmax, double ymax, Object object)
    { 
        Vector<Object> objects = mrootquad.getIntersected( xmin, ymin, xmax, ymax );
        if (objects.size() == 0)
           add(xmin, ymin, xmax, ymax, object );
		return (objects.size() == 0 ? false : true);
    }

	public Vector<Object> getIntersected(double xmin, double ymin, double xmax, double ymax)
	{
		return mrootquad.getIntersected( xmin, ymin, xmax, ymax );
	};

   /*************************************************************************
    *  test client
    *************************************************************************/
    public static void main(String[] args) {
		QuadTree tree = new QuadTree();
		Random r = new Random();
		for (int i=0; i < 50; i++)
		{
			double x1 = r.nextDouble();
			double x2 = r.nextDouble();
			double y1 = r.nextDouble();
			double y2 = r.nextDouble();
			double xmin = x1 < x2 ? x1 : x2;
			double xmax = x1 > x2 ? x1 : x2;
			double ymin = y1 < y2 ? y1 : y2;
			double ymax = y1 > y2 ? y1 : y2;

			tree.add(xmin,ymin,xmax,ymax,"C"+i);
		}
		Vector<Object> intersect = tree.getIntersected(0.1,0.1,0.2,0.2);
		System.out.println("Intersect :"+ intersect);
    }
}
