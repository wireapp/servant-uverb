module Servant.API.UVerb
  ( UVerb
  , HasStatus, getStatus
  , MakesResource, mkResource
  , MakesUVerb
  , respond
  , module Servant.API.UVerb.OpenUnion
  ) where

import Data.SOP.NS
import Data.SOP.Constraint
import Network.HTTP.Types (Status)
import Servant.API (StdMethod)
import Servant.API.UVerb.OpenUnion


{- | An alternative to 'Verb' for end-points that respond with a resource value of any of an
open union of types, and specific status codes for each type in this union.  (`UVerb` is short
for `UnionVerb`)

This can be used for returning (rather than throwing) exceptions in a server as in, say
@'[Report, WaiError]@; or responding with either a 303 forward with a location header, or 201
created with a different body type, depending on the circumstances.  (All of this can be done
with vanilla servant-server, but it can't be represented in the API types.)

See 'MakesResource' docs for an explanation of @mkres@.  Like in 'Verb', @method@ is the HTTP
method and @cts@ is the list of response content types, resp.


=== Design rationale

You could also use open union types in combination with 'Verb' rather than 'UVerb', but only
if all members of the union have the same response status code.

It would have been nice to have individual content type lists for different resources in the
union, perhaps something like:

>>> data UVerb (mkres :: * -> *) (method :: StdMethod) (resources :: [*])
>>>
>>> class HasCts (resource :: *) where
>>>   type Cts resource :: [*]

This has two obvious implementations which are equally bad:

(a) Content negotiation happens with 'addAcceptCheck' before the handler is called.  Then we
    don't know what the supported response content types will be, because that depends on what
    item of the union the handler will return later.  We could compute the intersection of all
    items in the union and process the accept header based on that, but that seems complicated
    and weird.

(b) Content negotiation happens *after* the handler is called, but 'fmap'-ping a function @::
    Route a -> Route a@ into the 'Delayed' response.  Then the handler would be called with
    all the effects it may have (touching the database, sending out emails, ...) by the time
    the response is @406 bad Accept header@.  This is at best confusing.

What we would need is a handler monad that supports commit and rollback of all effects in
'runDelayed', after the handler has been executed.  This is an interesting project, but it
escapes the scope of this library.  So we force every member of the union to implement all
content type encodings.

-}
data UVerb (mkres :: * -> *) (method :: StdMethod) (cts :: [*]) (resources :: [*])

{- | Associate every resource mentioned in a 'UVerb' resources list with its status code.

'getStatus' takes a proxy because some use cases (eg., servant-uverb-swagger,
servant-uverb-client) have no resource values to pass in here.

See 'MakesResource' docs for an explanation of @mkres@.


=== Design rationale

You may want to add the status info directly to the resource type, rather than having to go to
a place outside the routing table and write a very boring instance the fixes it.  This way you
could also use the same resource type with two different status codes in two different
end-points.

We could accomplish that in a way similar to 'Header'.  The difference is that the header name
in the type corresponds to a header value that needs to be set by the handler at run-time; but
the status code in the type is phantom, and doesn't correspond to anything the handler does.
So in the handler, we have to repetitively add the status code either as a type application or
as a proxy argument.

This can certainly be done, but we chose the path where writing the routing table is
syntactically a little less elegant, but in return the author of the handler only needs to
provide information that is not already determined statically.

-}
class HasStatus (mkres :: * -> *) (resource :: *) where
  getStatus :: forall (proxy :: * -> *). proxy (mkres resource) -> Status

{- | @mkres@ is usually a @newtype@ wrapper, but it needs to be introduced near the application
types so you can write 'HasStatus' instances of @mkres T@ for any application type @T@ without
introducing orphans.  Having this newtype lets you do this for as many @T@ as you need at the
cost of a single @newtype@.

'MakeResource' is the type class that lets the 'UVerb' mechanics wrap a value of any
application type into that newtype.

-}
class MakesResource (mkres :: * -> *) (resource :: *) where
  mkResource :: resource -> mkres resource

type MakesUVerb mkres method cts resources =
  ( All (HasStatus mkres) resources
  , All (MakesResource mkres) resources
  )

-- | 'return' for 'UVerb' handlers.  Pass it a value of an application type from the routing
-- table, and it will return a value of the union of responses.
respond
  :: forall (f :: * -> *) (mkres :: * -> *) (x :: *) (xs :: [*]).
     (Applicative f, MakesResource mkres x, IsMember x xs)
  => x -> f (NS mkres xs)
respond = pure . inject . mkResource
