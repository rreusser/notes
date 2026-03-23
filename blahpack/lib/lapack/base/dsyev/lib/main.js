

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsyev = require( './dsyev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyev, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyev;
