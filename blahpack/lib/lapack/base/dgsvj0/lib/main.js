

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgsvj0 = require( './dgsvj0.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgsvj0, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgsvj0;
