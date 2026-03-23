

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dormlq = require( './dormlq.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormlq, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormlq;
