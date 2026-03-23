

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlas2 = require( './dlas2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlas2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlas2;
