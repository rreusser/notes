

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlansy = require( './dlansy.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlansy, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlansy;
