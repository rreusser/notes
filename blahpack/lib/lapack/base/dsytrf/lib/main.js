

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsytrf = require( './dsytrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytrf;
