

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlabrd = require( './dlabrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlabrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlabrd;
