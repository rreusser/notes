

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlatrd = require( './dlatrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlatrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlatrd;
