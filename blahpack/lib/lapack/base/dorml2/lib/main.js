

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorml2 = require( './dorml2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorml2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorml2;
