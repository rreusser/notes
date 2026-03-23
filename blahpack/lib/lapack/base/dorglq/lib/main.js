

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorglq = require( './dorglq.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorglq, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorglq;
