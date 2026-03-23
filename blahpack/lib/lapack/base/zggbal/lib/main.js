

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zggbal = require( './zggbal.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zggbal, 'ndarray', ndarray );


// EXPORTS //

module.exports = zggbal;
