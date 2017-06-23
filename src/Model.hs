module Model where

import           Data.List.NonEmpty

data Calling = Call String [String] | Csh [String]

data Group

class RefIndex a where
  findIn :: Group -> a

data VertexGeo n = VertexGeo n n n (Maybe n)
data VertexTexture n = VertexTexture n (Maybe n) (Maybe n)
data VertexNomal n = VertexNomal n n n

data RefVertexGeo n = RefVertexGeo Int
data RefVertexTexture n = RefVertexTexture Int
data RefVertexNormal n = RefVertexNormal Int

type RefVertices n = NonEmpty (RefVertexGeo n)
type RefVerticesT n = NonEmpty (RefVertexGeo n
  , Maybe (RefVertexTexture n))
type RefVerticesTN n = NonEmpty (RefVertexGeo n
  , Maybe (RefVertexTexture n)
  , Maybe (RefVertexNormal n))

data Elements n =
  Points (RefVertices n)
  | Lines (RefVerticesT n)
  | Faces (RefVerticesTN n)

data FreeFormAttributes n cs = FreeFormAttributes CsType
  (FreeFormPart n cs)
  (FreeFormPart n cs)
  (FreeFormPart n cs)
data CsType = BMatrix | Bezier | BSpline | Cardinal | Taylor
data FreeFormPart n cs = Deg (cs n) | Step (cs n) | BMat (cs (Matrix n))

data FreeCurve a = FreeCurve a
data FreeSurface a = FreeSurface a a

data Matrix a = Matrix [a]

data Surface n = Surface n n n n (RefVerticesTN n)
data RefSurface n = RefSurface Int

data Curve n = Curve n n (NonEmpty Int)

data Curve2 n cs = Curve2 (NonEmpty (RefVertexParamSpace n cs))
data RefCurve2 n cs = RefCurve2 Int

data VertexParamSpace n cs = VertexParamSpace (cs n) (Maybe n)
data RefVertexParamSpace n cs = RefVertexParamSpace Int

data FreeFormStatements n cs = FFEnd
  | FFParamU n (NonEmpty n)
  | FFParamV n (NonEmpty n)
  | FFTrim (NonEmpty (GrabCurve2d n cs))
  | FFHole (NonEmpty (GrabCurve2d n cs))
  | FFScrv (NonEmpty (GrabCurve2d n cs))
  | FFSp (NonEmpty (RefCurve2 n cs))

data GrabCurve2d n cs = GrabCurve2d n n (RefCurve2 n cs)

data GrabSurfaces n cs = GrabSurfaces (RefSurface n) n n (RefCurve2 n cs)
data ConSurfaces n cs = ConSurfaces (GrabSurfaces n cs) (GrabSurfaces n cs)

data Grouping n = Group (NonEmpty String)
  | Smooth (Maybe Int)
  | Merge (Maybe (Int, n))
  | OptionalGroup String

data Rendering n = PolyOnly PolyOnly
  | PolyOrFreeForm PolyOrFreeForm
  | FreeFormOnly (FreeFormOnly n)

data PolyOnly = Bevel Bool | CInterp Bool | DInterp Bool
data PolyOrFreeForm = Lod Int
  | MapLib (NonEmpty String)
  | UserMap (Maybe String)
  | UseMtl String
  | MtlLib (NonEmpty String)
  | ShadowObj String
  | TraceObj String
data FreeFormOnly n = CTech (CTech n) | STech (STech n)

data CTech n = CTechCParm n
  | CTechCSpace n
  | CTechCurv n n
data STech n = STechCParmA n n
  | STechCParamB n
  | STechCSpace n
  | STechCurv n n
